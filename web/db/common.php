<?php

require __DIR__ . "/../config.php";

# Single share connection
$pdo=null;

# sections to show in menu (subfolders)
$sections=array(
	"citations" => array(
		"section" => "BETY",
		"list" => "SELECT id, author, title FROM citations",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"dbfiles" => array(
		"section" => "BETY",
		"list" => "SELECT dbfiles.id as id, machines.hostname as machine, dbfiles.file_path as filepath, dbfiles.file_name as filename FROM dbfiles, machines WHERE dbfiles.machine_id=machines.id",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"formats" => array(
		"section" => "BETY",
		"list" => "SELECT id, name, mime_type FROM formats",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"inputs" => array(
		"section" => "BETY",
		"list" => "SELECT inputs.id AS id, inputs.name AS name, sites.sitename AS sitename, count(dbfiles.id) AS files FROM inputs".
				  " LEFT JOIN sites ON sites.id = inputs.site_id" .
				  " LEFT JOIN dbfiles ON dbfiles.container_id = inputs.id AND dbfiles.container_type='Input'" . 
				  " GROUP BY inputs.id, sites.sitename",
		"files" => TRUE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"sites" => array(
		"section" => "BETY",
		"list" => "SELECT id, sitename, city, state, country FROM sites",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"species" => array(
		"section" => "BETY",
		"list" => "SELECT id, genus, species, scientificname FROM species",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"traits" => array(
		"section" => "BETY",
		"list" => "SELECT traits.id AS id, sites.sitename AS sitename, species.commonname AS speciename, statname FROM traits".
				  " LEFT JOIN sites ON sites.id = traits.site_id" .
				  " LEFT JOIN species ON species.id = traits.specie_id",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"users" => array(
		"section" => "BETY",
		"list" => "SELECT id, login, name, email FROM users",
		"files" => FALSE,
		"level" => array(
			"show" => 1,
			"edit" => 1,
		)
	),
	"ensembles" => array(
		"section" => "PEcAn",
		"list" => "SELECT id, runtype, workflow_id FROM ensembles",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"models" => array(
		"section" => "PEcAn",
		"list" => "SELECT models.id, models.model_name AS name, models.revision, count(dbfiles.id) AS files FROM models" .
				  " LEFT JOIN dbfiles ON dbfiles.container_id = models.id AND dbfiles.container_type='Model'" . 
				  " GROUP BY models.id",
		"files" => TRUE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"runs" => array(
		"section" => "PEcAn",
		"list" => "SELECT workflows.id as id, CONCAT(sitename, ', ', city, ', ', state, ', ', country) as site, models.model_name as model, workflows.started_at as start_date, workflows.finished_at as end_date FROM workflows, sites, models WHERE workflows.site_id=sites.id AND workflows.model_id=models.id",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	),
	"workflows" => array(
		"section" => "PEcAn",
		"list" => "SELECT runs.id as id, CONCAT(sitename, ', ', city, ', ', state, ', ', country) AS site, CONCAT(model_name, ' (', revision, ')') AS model, runs.started_at as start_date, runs.finished_at as end_date FROM runs, sites, models WHERE runs.site_id=sites.id AND runs.model_id=models.id",
		"files" => FALSE,
		"level" => array(
			"show" => 4,
			"edit" => 3,
		)
	)
);

# make sure we do a session start
session_start();

# ----------------------------------------------------------------------
# DATABASE FUNCTIONS
# ----------------------------------------------------------------------
function open_database() {
	global $db_bety_hostname;
	global $db_bety_username;
	global $db_bety_password;
	global $db_bety_database;
	global $db_bety_type;
	global $pdo;

	$pdo = new PDO("${db_bety_type}:host=${db_bety_hostname};dbname=${db_bety_database}", $db_bety_username, $db_bety_password);
}

function close_database() {
	global $pdo;
	$pdo = null;
}

function error_database() {
	global $pdo;
	$tmp = $pdo->errorInfo();
	return $tmp[2];
}

function column_names($table) {
	global $pdo;

	$rs = $pdo->query("SELECT * FROM ${table} LIMIT 0");
	for ($i = 0; $i < $rs->columnCount(); $i++) {
		$col = $rs->getColumnMeta($i);
		$columns[$col['name']] = $col['native_type'];
	}
	$rs->closeCursor();
	return $columns;
}

# ----------------------------------------------------------------------
# COMMON HTML FUNCTIONS
# ----------------------------------------------------------------------
function print_header($table="") {
	global $sections;

	open_database();

	print "<html>\n";
	print "<head>\n";
	if ($table == "") {
		print "<title>PEcAn DB</title>\n";
	} else {
		print "<title>PEcAn DB [{$sections[$table]['section']}/{$table}]</title>\n";
	}
	print "<link href=\"bety.css\" rel=\"stylesheet\" type=\"text/css\"/>\n";
	print "</head>\n";
	print "<body>\n";
}

function print_footer($msg="") {
	if ($msg != "") {
		print "<hr/>\n";
		print "$msg\n";
	}
	print "<hr/>\n";
	print "<div style='float: left'><a href='http://pecanproject.org'>PEcAn Project</a></div>\n";
	print "<div style='float: right'><a href='http://betydb.org'>BETY Project</a></div>\n";
	print "</body>\n";
	print "</html>\n";

	close_database();
}

function print_menu($active) {
	global $sections;

	$menu=array("Home" => "index.php");

	foreach($sections as $key => $entry) {
		$section = $entry['section'];
		# make sure there is an entry for the section
		if (empty($menu[$section])) {
			$menu[$section] = array();
		}

		# Add the entry
		if (get_page_acccess_level() <= $entry['level']['show']) {
			$menu[$section][$key]["#"] = "list.php?table={$key}";

			# if edit capabilities add new option
			if (get_page_acccess_level() <= $entry['level']['edit']) {
				$menu[$section][$key]["New"] = "edit.php?table={$key}&id=-1";
			}
		}
	}

	if (check_login()) {
		$menu[get_user_name()] = array(
			"Edit" => "edit.php?table=users&id=" . get_userid(),
			"Logout" => "logout.php",
			);
	} else {
		$menu["Login"] = "login.php";
	}

	print "<div id='cssmenu'>\n";
	print_menu_entry($active, $menu);
	print "</div><br/>\n";
}

function print_menu_entry($active, $menu) {
	$keys = array_keys($menu);
	$last = end($keys);
	print "<ul>\n";
	foreach($menu as $key => $val) {
		if ($key == "#") {
			continue;
		}
		$class = "";
		if ($active == $key) {
			$class .= " active";			
		}
		if (is_array($val)) {
			$class .= " has-sub";	
		}
		if ($last == $key) {
			$class .= " last";
		}
		$class=trim($class);
		if ($class != "") {
			print "<li class='$class'>";
		} else {
			print "<li>";
		}
		if (is_array($val)) {
			if (array_key_exists("#", $val)) {
				$url = $val['#'];
			} else {
				$url = "#";
			}
			print "<a href='$url'><span>$key</span></a>";
		} else if ($val != "") {
			print "<a href='$val'><span>$key</span></a>";
		} else {
			print "<span>$key</span>";
		}
		if (is_array($val)) {
			print "\n";
			print_menu_entry($active, $val);
		}
		print "</li>\n";
	}
	print "</ul>\n";
}

# ----------------------------------------------------------------------
# USER FUNCTIONS
# ----------------------------------------------------------------------

function login($username, $password) {
	global $pdo;

	if (isset($_SESSION['userid'])) {
		return TRUE;
	}

	$q=$pdo->prepare("SELECT * FROM users WHERE login=:username");
	$q->bindParam(':username', $username, PDO::PARAM_STR);
	if ($q->execute() === FALSE) {
	  die('Invalid query : ' . error_database());
	}
	$row = $q->fetch(PDO::FETCH_ASSOC);
	$q->closeCursor();

	if (!isset($row['salt'])) {
		return FALSE;
	}

	$digest = encrypt_password($password, $row['salt']);

	if ($digest == $row['crypted_password']) {
		$_SESSION['userid']=$row['id'];
		$_SESSION['username']=$row['name'];
		$_SESSION['useraccess']=$row['access_level'];
		$_SESSION['userpageaccess']=$row['page_access_level'];
		return TRUE;
	} else {
		return FALSE;
	}
}

function encrypt_password($password, $salt) {
	global $REST_AUTH_SITE_KEY;
	global $REST_AUTH_DIGEST_STRETCHES;

	$digest=$REST_AUTH_SITE_KEY;
	for($i=0; $i<$REST_AUTH_DIGEST_STRETCHES; $i++) {
	  $digest=sha1($digest . "--" . $salt . "--" . $password . "--" . $REST_AUTH_SITE_KEY);
	}
	return $digest;	
}

function logout() {
	unset($_SESSION['userid']);
	unset($_SESSION['username']);
	unset($_SESSION['useraccess']);
	unset($_SESSION['userpageaccess']);
}

function get_userid() {
	if (isset($_SESSION['userid'])) {
		return $_SESSION['userid'];
	} else {
		return -1;
	}
}

function check_login() {
	return isset($_SESSION['userid']);
}

function get_user_name() {
	if (isset($_SESSION['username'])) {
		return $_SESSION['username'];
	} else {
		return FALSE;
	}
}

function get_acccess_level() {
	global $anonymous_level;
	if (isset($_SESSION['useraccess'])) {
		return $_SESSION['useraccess'];
	} else {
		return $anonymous_level;
	}
}

function get_page_acccess_level() {
	global $anonymous_page;
	if (isset($_SESSION['userpageaccess'])) {
		return $_SESSION['userpageaccess'];
	} else {
		return $anonymous_page;
	}
}

# ----------------------------------------------------------------------
# LIST PAGE FUNCTIONS
# ----------------------------------------------------------------------

function print_list($table, $query) {
	global $pagesize;
	global $sections;
	global $pdo;

	$idkey = 'id';

	# handle any information send in inputs form
	$msg = "";
	if (isset($_REQUEST['action'])) {
		if ($_REQUEST['action'] == "delete") {
			if (($table != "users") || ((get_page_acccess_level() == 1) && ($_REQUEST['kill'] != get_userid()))) {
				$q = "DELETE FROM $table WHERE ${idkey}={$_REQUEST['kill']};";
				if ($pdo->query($q) === FALSE) {
					$msg = "Error updating database : " . error_database() . "<br>{$q}";
					editor_log("FAIL", $q);
				} else {
					$msg .= "Removed {$_REQUEST['kill']} from {$table}";
					editor_log("OK", $q);
				}
			}
		}
	}

	if (isset($_REQUEST['pagesize'])) {
		$pagesize = $_REQUEST['pagesize'];
	}

	# fix access_level
	if (($table != "users") && (get_page_acccess_level() > 1)) {
		if (in_array('access_level', array_keys(column_names($table)))) {
			$pos = stripos($query, "WHERE");
			if ($pos !== false) {
				$head = substr($query, 0, $pos + 5);
				$tail = substr($query, $pos + 6);
				$query = "$head (access_level >= " . get_acccess_level() . " OR access_level IS NULL) AND $tail";
			} else {
				$pos = stripos($query, "group");
				if ($pos ) {
					$head = substr($query, 0, $pos);
					$tail = substr($query, $pos);
					$query = "$head WHERE (access_level >= " . get_acccess_level() . " OR access_level IS NULL) $tail";
				} else {
					$query .= " WHERE (access_level >= " . get_acccess_level() . " OR access_level IS NULL)";
				}
			}
		}
	}

	# get the input that we want to show
	if (isset($_REQUEST['page'])) {
		$current = $_REQUEST['page'];
	} else {
		$current = 1;
	}
	$result = $pdo->query($query . " ORDER BY $idkey LIMIT $pagesize OFFSET " . (($current - 1) * $pagesize));
	if (!$result) {
		die("Invalid query : $query " . error_database());
	}


	print "<div class=\"tbl\" id=\"list\">\n";
	print "	<div class=\"row\">\n";
	print "	  <div class=\"hdr id\">Action</div>\n";
	for ($i = 0; $i < $result->columnCount(); $i++) {
		$key = $result->getColumnMeta($i)['name'];
		if ($key == $idkey) {
			print "	  <div class=\"hdr id\">$key</div>\n";
		}
	}
	for ($i = 0; $i < $result->columnCount(); $i++) {
		$key = $result->getColumnMeta($i)['name'];
		if ($key != $idkey) {
			print "	  <div class=\"hdr $key\">$key</div>\n";
		}
	}
	print "	</div>\n";

	while($row = @$result->fetch(PDO::FETCH_ASSOC)) {
		print "	<div class=\"row\">\n";
		if (array_key_exists($idkey, $row)) {
			print "	  <div class=\"col id\">";
			print "<a title=\"Show {$row[$idkey]} in {$table}\" href=\"show.php?table={$table}&id={$row[$idkey]}\">S</a> ";
			if (get_page_acccess_level() <= $sections[$table]['level']['edit']) {
				print "<a title=\"Edit {$row[$idkey]} in {$table}\"  href=\"edit.php?table={$table}&id={$row[$idkey]}\">E</a> ";
			}
			if (get_page_acccess_level() <= $sections[$table]['level']['edit'] && (($table != "users") || ($row[$idkey] != get_userid()))) {
				$url="list.php?table={$table}&page={$current}&action=delete&kill={$row[$idkey]}";
				print "<a title=\"Remove {$row[$idkey]} frome {$table}\"  href=\"$url\" onclick=\"return confirm('Are you sure you want to delete item {$row[$idkey]}?')\">D</a> ";
			}
			print "</div>\n";
			print "	  <div class=\"col id $idkey\">{$row[$idkey]}</div>\n";
		}
		foreach ($row as $key => $value) {
			if ($key != $idkey) {
				print "	  <div class=\"col $key\">$value</div>\n";
			}
		}
		print "  </div>\n";
	}
	print "</div>\n";

	$result->closeCursor();

	print_pages($current, $pagesize, $query, $table);

	return $msg;
}

function print_pages($current, $pagesize, $query, $table) { 
	global $pdo;

	# count items
	$result = $pdo->query($query);
	if (!$result) {
		die('Invalid query : ' . error_database());
	}
	$count = $result->rowCount();
	$result->closeCursor();

	if ($count <= $pagesize) {
		return;
	}

	$pages = "";
	if ($count > 0) { 
		$numpages = ceil($count / $pagesize);

		if ($numpages <= 15) {
			for ($i=1; $i<$numpages+1; $i++) { 
				if ($i == $current) { 
					$pages .= " <b>$i</b> "; 
				} else { 
					$pages .= " <a href=\"{$table}_list.php?page=$i\">$i</a> "; 
				} 
			}			
		} else {
			if ($current < 8) {
				for ($i=1; $i<12; $i++) {
					if ($i == $current) { 
						$pages .= " <b>$i</b> "; 
					} else { 
						$pages .= " <a href=\"{$table}_list.php?page=$i\">$i</a> "; 
					} 
				}
				$pages .= "...";
				for ($i=$numpages-2; $i<$numpages; $i++) {
					$pages .= " <a href=\"{$table}_list.php?page=$i\">$i</a> ";
				}				
			} else {
				for ($i=1; $i<3; $i++) {
					$pages .= " <a href=\"{$table}_list.php?page=$i\">$i</a> ";
				}
				$pages .= "...";
				if ($current > ($numpages - 7)) {
					for ($i=$numpages-10; $i<$numpages; $i++) {
						if ($i == $current) { 
							$pages .= " <b>$i</b> "; 
						} else {
							$pages .= " <a href=\"{$table}_list.php?page=$i\">$i</a> "; 
						}
					}			
				} else {
					for ($i=$current-4; $i<$current+5; $i++) {
						if ($i == $current) { 
							$pages .= " <b>$i</b> "; 
						} else {
							$pages .= " <a href=\"{$table}_list.php?page=$i\">$i</a> "; 
						} 
					}
					$pages .= "...";
					for ($i=$numpages-2; $i<$numpages; $i++) {
						$pages .= " <a href=\"{$table}_list.php?page=$i\">$i</a> ";
					}				
				}
			}
		}
	}

	print "<p>";
	if ($pages != "") {
		if ($current > 1) {
			$pages = "<a href=\"{$table}_list.php?page=" . ($current-1) . "\">&lt;</a> $pages";
		} else {
			$pages = "&lt; $pages";
		}
		if ($current < $numpages) {
			$pages = "$pages <a href=\"{$table}_list.php?page=" . ($current+1) . "\">&gt;</a>";				
		} else {
			$pages = "&gt; $pages";
		}
		print "<div align=\"left\">$pages</div>";
	}
	print "</p>\n";
}

# ----------------------------------------------------------------------
# SHOW FILES ASSOCIATED
# ----------------------------------------------------------------------

function show_files($id, $table, $readonly) {
	global $pdo;
	global $sections;

	$type = ucfirst(strtolower(substr($table, 0, -1)));

	# process the form
	$msg = "";
	if (!$readonly && isset($_REQUEST['action'])) {
		if ($_REQUEST['action'] == "add") {
			$query = "UPDATE dbfiles SET container_id={$id}, container_type='{$type}' WHERE id={$_REQUEST['dbid']};";
			if ($pdo->query($query) === FALSE) {
				$msg = "Error updating database : [" . error_database() . "] " . $pdo->errorInfo($pdo) . "<br>";
				editor_log("FAIL", $query);
			} else {
				$msg .= "Added dbfiles={$_REQUEST['dbid']} to inputs={$_REQUEST['id']}<br/>\n";
				editor_log("OK", $query);
			}
		}

		if ($_REQUEST['action'] == "del") {
			$query = "UPDATE dbfiles SET container_id=NULL WHERE id={$_REQUEST['dbid']};";
			if ($pdo->query($query) === FALSE) {
				$msg = "Error updating database : [" . error_database() . "] " . $pdo->errorInfo($pdo) . "<br>";
				editor_log("FAIL", $query);
			} else {
				$msg .= "Removed dbfiles={$_REQUEST['dbid']} from inputs={$_REQUEST['id']}<br/>\n";
				editor_log("OK", $query);
			}
		}
	}

	print "	<hr />\n";
	print "	Existing Files<br/>\n";

	# get the input that we want to show
	$query="SELECT dbfiles.id, concat(machines.hostname, ':', dbfiles.file_path, '/', dbfiles.file_name) as filename" .
		   " FROM dbfiles, machines" .
		   " WHERE dbfiles.container_id={$id} AND dbfiles.container_type='{$type}' AND machines.id=dbfiles.machine_id;";
	$result = $pdo->query($query);
	if (!$result) {
		die("Invalid query [$query] " . $pdo->errorInfo($pdo));
	}
	if ($result->rowCount() > 0) {
		print "		<script type=\"text/javascript\">\n";
		if (!$readonly) {			
			print "		function add_input(id) {\n";
			print "			var e = document.getElementById(id);\n";
			print "			var v = e.options[e.selectedIndex].value;\n";
			print "			window.location.href = \"edit.php?table={$table}&{$id}&action=add&dbid=\" + v;\n";
			print "		}\n";
			print "		function del_file(id) {\n";
			print "			var e = document.getElementById(id);\n";
			print "			var v = e.options[e.selectedIndex].value;\n";
			print "			window.location.href = \"edit.php?table={$table}&{$id}&action=del&dbid=\" + v;\n";
			print "		}\n";
			print "		function edit_file(id) {\n";
			print "			var e = document.getElementById(id);\n";
			print "			var v = e.options[e.selectedIndex].value;\n";
			print "			window.location.href = \"edit.php?table=dbfiles&id=\" + v;\n";
			print "		}\n";
		}
		print "			function show_file(id) {\n";
		print "				var e = document.getElementById(id);\n";
		print "				var v = e.options[e.selectedIndex].value;\n";
		print "				window.location.href = \"show.php?table=dbfiles&id=\" + v;\n";
		print "			}\n";
		print "		</script>\n";
		print "		<div class=\"tbl\" id=\"editor\">\n";
		print "			<div class=\"row\">\n";
		print "				<div class=\"key\">\n";
		print "					<a href=\"javascript:void(0)\" title=\"Show DBFile\" onclick=\"show_file('file')\">S</a>\n";
		if (!$readonly) {
			print "					<a href=\"javascript:void(0)\" title=\"Edit DBFile\" onclick=\"edit_file('file')\">E</a>\n";
			print "					<a href=\"javascript:void(0)\" title=\"Disassociate DBFile\" onclick=\"del_file('file')\">D</a>\n";
		}
		print "				</div>\n";
		print "				<div class=\"val\">\n";
		print "					<select id=\"file\">\n";
		while($dbfilerow = $result->fetch(PDO::FETCH_ASSOC)) {
			print "<option value=\"{$dbfilerow['id']}\">{$dbfilerow['filename']}</option>\n";
		}
		$result->closeCursor();
		print "					</select>\n";
		print "				</div>\n";
		print "			</div>\n";
		print "		</div>\n";
	}

	if (!$readonly) {
		print "	<hr />\n";
		print "	Add a Files<br/>\n";

		# get the input that we want to show
		$query="SELECT dbfiles.id, concat(machines.hostname, ':', dbfiles.file_path, '/', dbfiles.file_name) as filename" .
			   " FROM dbfiles, machines" .
			   " WHERE dbfiles.container_id IS NULL AND machines.id=dbfiles.machine_id;";
		$result = $pdo->query($query);
		if (!$result) {
			die("Invalid query [$query] " . $pdo->errorInfo($pdo));
		}
		if ($result->rowCount() > 0) {
			print "		<div class=\"tbl\" id=\"editor\">\n";
			print "			<div class=\"row\">\n";
			print "				<div class=\"key\">\n";
			print "					<a href=\"javascript:void(0)\" title=\"Show DBFile\" onclick=\"show_file('addfile')\">S</a>\n";
			if (!$readonly) {
				print "					<a href=\"javascript:void(0)\" title=\"Edit DBFile\" onclick=\"edit_file('addfile')\">E</a>\n";
				print "					<a href=\"javascript:void(0)\" title=\"Associate DBFile\" onclick=\"del_file('addfile')\">A</a>\n";
			}
			print "				</div>\n";
			print "				<div class=\"val\">\n";
			print "					<select id=\"addfile\">\n";
			while($dbfilerow = $result->fetch(PDO::FETCH_ASSOC)) {
				print "<option value=\"{$dbfilerow['id']}\">{$dbfilerow['filename']}</option>\n";
			}
			$result->closeCursor();
			print "					</select>\n";
			print "				</div>\n";
			print "			</div>\n";
			print "		</div>\n";
		}
	}

	return $msg;
}



# ----------------------------------------------------------------------
# EDIT PAGE FUNCTIONS
# ----------------------------------------------------------------------

function editor_log($status, $query) {
	global $logfile;
	if (is_writeable($logfile)) {
		file_put_contents($logfile, date("c") . "\t${status}\t" . get_userid() . "\t" . get_user_name() . "\t${query}\n", FILE_APPEND);
	}
}

function editor_update($id, $table) {
	global $pdo;

	# get the row from the database (this can be empty)
	$result = $pdo->query("SELECT * FROM $table WHERE id=$id;");
	if (!$result) {
		die('Invalid query : ' . error_database());
	}
	$row = $result->fetch(PDO::FETCH_ASSOC);
	$result->closeCursor();

	$msg = "";
	$set = "";
	foreach($_REQUEST as $key => $val) {
		if ($key == "id") continue;
		if ($key == "table") continue;
		if ($key == "action") continue;

		$pre = substr($key, 0, 1);
		$key = substr($key, 2);

		if ($val == $row[$key]) {
			continue;
		}

		if ($pre == 's') {
			if ($set != "") {
				$set .= ", ";
			}
			if ($val == "") {
				$set .= " $key=NULL";
			} else {
				$set .= " $key=" . $pdo->quote($val);
			}
		} else if ($pre == 'n') {
			if ($set != "") {
				$set .= ", ";
			}
			$set .= " $key=" . $pdo->quote($val);
		} else if ($pre == 'b') {
			if ($set != "") {
				$set .= ", ";
			}
			if ($val == 'on') {
				$set .= " $key=true";
			} else {
				$set .= " $key=false";
			}
		} else if ($pre == 'u') {
			if ($set != "") {
				$set .= ", ";
			}
			$set .= " $key=NOW()";				
		} else if ($pre == 'p') {
			$salt = $_REQUEST['s_salt'];
			if (($val != "") && ($salt != "")) {
				$val = encrypt_password($val, $salt);
				if ($set != "") {
					$set .= ", ";
				}
				$set .= " $key=" . $pdo->quote($val);
			}
		}
	}
	if ($set != "") {
		if ($id == "-1") {
			$query = "INSERT INTO $table SET $set;";
			$pdo->query($query, $param);
			$id = $pdo->lastInsertId();
			if (!$pdo->query($query)) {
				$msg = "Error updating database : " . error_database() . "<br/>$query";
				editor_log("FAIL", $query);
			} else {
				$msg .= "Added into $table table id=$id<br/>\n";
				editor_log("OK", $query);
			}
		} else {
			$query = "UPDATE $table SET $set WHERE id=$id;";
			if (!$pdo->query($query)) {
				$msg = "Error updating database : " . error_database() . "<br/>$query";
				editor_log("FAIL", $query);
			} else {
				$msg .= "Updated $table table for id=$id<br/>\n";
				editor_log("OK", $query);
			}
		}
	} else {
		if ($id == "-1") {
			$result = $pdo->query("SELECT id FROM $table ORDER BY id ASC LIMIT 1;");
			if (!$result) {
				die('Invalid query : ' . error_database());
			}
			$row = $result->fetch(PDO::FETCH_ASSOC);
			$id = $row[0];
			$result->closeCursor();
			$msg .= "No data entered showing first $table.<br/>\n";
		} else {
			$msg .= "Nothing changed.<br/>\n";
		}
	}

	return $msg;
}

function print_entry($id, $table, $readonly) {
	global $pdo;
	global $sections;

	$edit = false;
	if ($readonly) {
		if ($sections[$table]['level']['show'] < get_page_acccess_level() && ($table != "users" || $id != get_userid())) {
			#header("Location: index.php");
			die("no access");
		}
		if ($sections[$table]['level']['edit'] >= get_page_acccess_level() || ($table == "users" && $id == get_userid())) {
			$edit = true;
		}
	} else {
		if ($sections[$table]['level']['edit'] < get_page_acccess_level() && ($table != "users" || $id != get_userid())) {
			#header("Location: index.php");
			die("no access");
		}
	}

	# update database
	$msg = "";
	if (!$readonly && isset($_REQUEST['action']) && $_REQUEST['action'] == "update") {
		$msg = editor_update($id, $table);
	}

	# print navigation
	print_prev_next($id, $table, $edit);

	if ($readonly) {
		$disabled = "disabled";
	} else {
		$disabled = "";
	}

	# get the row from the database (this can be empty)
	$result = $pdo->query("SELECT * FROM $table WHERE id=$id;");
	if (!$result) {
		die('Invalid query : ' . error_database());
	}
	$row = $result->fetch(PDO::FETCH_ASSOC);
	$result->closeCursor();

	# check access_level
	if (is_array($row) && array_key_exists('access_level', $row) && ($row['access_level'] != "") && ($row['access_level'] != "-1")) {
		if (get_acccess_level() > $row['access_level']) {
			header("Location: index.php");
			return;
		}
	}

	if (!$readonly) {
		print "<form method=\"post\" action=\"edit.php\">\n";
		print "<input name=\"table\" type=\"hidden\" value=\"$table\"/>\n";
		print "<input name=\"id\" type=\"hidden\" value=\"$id\"/>\n";
	}
	print "<div class=\"tbl\" id=\"editor\">\n";

	foreach(column_names($table) as $key => $type) {
		if ($key == "id") {
			$fancykey = $key;
		} else {
			$fancykey = ucwords(str_replace("_", " ", str_replace("_id", "", $key)));
		}
		if (is_array($row) && array_key_exists($key, $row)) {
			$val = $row[$key];
		} else {
			$val = "";
		}

		if (substr($val, 0, 4) == "http") {
			$fancykey = "<a href=\"$val\">$fancykey</a>";
		}

		print "<div class=\"row\">\n";
		if ($key == "id") {
			if ($id == -1) {
				$val = "new entry";
			}
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><input name=\"$key\" type=\"text\" disabled value=\"$val\"/></div>\n";
		} else if ($key == "created_at") {
			if ($id == -1) {
				$val = 'now';
				print "<input name=\"u_$key\" type=\"hidden\" value=\"$val\"/>\n";
			}
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><input name=\"$key\" type=\"text\" disabled value=\"$val\"/></div>\n";
		} else if ($key == "updated_at") {
			if ($id != -1) {
				print "<input name=\"u_$key\" type=\"hidden\" value=\"$val\"/>\n";
			}
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><input name=\"$key\" type=\"text\" disabled value=\"$val\"/></div>\n";
		} else if ($key == "site_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_sites_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "model_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_models_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if (($key == "user_id") || ($key == "created_user_id") || ($key == "updated_user_id")) {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_users_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "machine_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_machines_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "format_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_formats_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "citation_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_citations_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "specie_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_species_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "variable_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_variables_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "treatment_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_treatments_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "cultivar_id") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_cultivars_options("n_$key", $val, $readonly);
			print "</div>\n";
		} else if ($key == "page_access_level") {
			if (get_page_acccess_level() == 1) {
				$sel_readonly=$readonly;
			} else {
				$sel_readonly=true;
			}
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_select_array_options("n_$key", $val, $sel_readonly, array(
				"1" => "Administrator",
				"2" => "Manager",
				"3" => "Creator",
				"4" => "Viewer"));
			print "</div>\n";
		} else if ($key == "access_level") {
			if ((get_page_acccess_level() == 1) || ($val == "") || ($val == "-1")) {
				$sel_readonly=$readonly;
			} else {
				$sel_readonly=true;
			}
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\">\n";
			print_select_array_options("n_$key", $val, $sel_readonly, array(
				"1" => "Restricted",
				"2" => "Internal EBI & Collaborators",
				"3" => "Creator",
				"4" => "Viewer"));
			print "</div>\n";
		} else if ($key == "salt") {
			if ($id == -1) {
				$val = uniqid("", true);
				print "<input name=\"s_$key\" type=\"hidden\" value=\"$val\"/>\n";
			} else {
				print "<input name=\"s_$key\" type=\"hidden\" value=\"$val\"/>\n";
			}
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><input name=\"s_$key\" type=\"text\" disabled value=\"$val\"/></div>\n";
		} else if ($key == "crypted_password") {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><input name=\"p_$key\" type=\"password\" $disabled value=\"\"/></div>\n";
		} else if (stristr($type, "text")) {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><textarea name=\"s_$key\" rows=\"10\" $disabled>$val</textarea></div>\n";
		} else if (stristr($type, "tinyint")) {			
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\" style=\"width: auto;\"><input name=\"b_$key\" type=\"checkbox\" style=\"width: auto;\" $disabled";
			if ($val == 1) {
				print " checked";
			}
			print "/></div>\n";
		} else {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><input name=\"s_$key\" type=\"text\" $disabled value=\"$val\"/></div>\n";
		}
		print "</div>\n";

	}

	if (!$readonly) {
		print "<div class=\"row\">\n";
		print "<div class=\"key\"><input name=\"action\" type=\"submit\" value=\"update\"/></div>\n";
		print "<div class=\"val\"></div>\n";
		print "</div>\n";
	}
	print "</div>\n";
	if (!$readonly) {
		print "</form>\n";
	}

	return $msg;
}

function print_prev_next($id, $table, $edit=false) {
	global $pdo;

	print "<div>\n";
	print "<a href=\"list.php?table={$table}\">All</a>";

	$and = "";
	$where = "";
	if (in_array('access_level', column_names($table))) {
		$and   = " AND (access_level >= " . get_acccess_level() . " OR access_level IS NULL)";
		$where = " WHERE (access_level >= " . get_acccess_level() . " OR access_level IS NULL)";
	}

	$result = $pdo->query("SELECT id FROM {$table} WHERE id < ${id} ${and} ORDER BY id DESC LIMIT 1;");
	if (!$result) {
		die('Invalid query : ' . error_database());
	}
	$row = $result->fetch(PDO::FETCH_NUM);
	$prev = $row[0];
	$result->closeCursor();
	if ($prev == "") {
		$result = $pdo->query("SELECT id FROM {$table} ${where} ORDER BY id DESC LIMIT 1;");
		if (!$result) {
			die('Invalid query : ' . error_database());
		}
		$row = $result->fetch(PDO::FETCH_NUM);
		$prev = $row[0];
		$result->closeCursor();
	}
	print "&nbsp;<a href=\"{$_SERVER['SCRIPT_NAME']}?table={$table}&id={$prev}\">Prev</a>";

	$result = $pdo->query("SELECT id FROM $table WHERE id > ${id} ${and} ORDER BY id ASC LIMIT 1;");
	if (!$result) {
		die('Invalid query : ' . error_database());
	}
	$row = $result->fetch(PDO::FETCH_NUM);
	$next = $row[0];
	$result->closeCursor();
	if ($next == "") {
		$result = $pdo->query("SELECT id FROM $table ${where} ORDER BY id ASC LIMIT 1;");
		if (!$result) {
			die('Invalid query : ' . error_database());
		}
		$row = $result->fetch(PDO::FETCH_NUM);
		$next = $row[0];
		$result->closeCursor();
	}
	print "&nbsp;<a href=\"{$_SERVER['SCRIPT_NAME']}?table={$table}&id={$next}\">Next</a>";

	if ($edit) {
		print "&nbsp;<a href=\"edit.php?table={$table}&id={$id}\">Edit</a>\n";
	} else if (strpos($_SERVER['SCRIPT_NAME'], 'show.php') === false) {
		print "&nbsp;<a href=\"show.php?table={$table}&id={$id}\">Show</a>\n";
	}

	print "</div><br/>\n";
}

function print_users_options($name, $myid, $readonly=false) {
	$query = "SELECT id, CONCAT(name, ' &lt;', email, '&gt;') AS name FROM users";
	print_select_options($name, $myid, $readonly, $query);
}

function print_machines_options($name, $myid, $readonly=false) {
	$query = "SELECT id, hostname AS name FROM machines";
	print_select_options($name, $myid, $readonly, $query);
}

function print_formats_options($name, $myid, $readonly=false) {
	$query = "SELECT id, name FROM formats";
	print_select_options($name, $myid, $readonly, $query);
}

function print_sites_options($name, $myid, $readonly=false) {
	$query = "SELECT id, CONCAT(coalesce(sitename, ''), ', ', coalesce(city, ''), ', ', coalesce(state, ''), ', ', coalesce(country, '')) AS name FROM sites";
	print_select_options($name, $myid, $readonly, $query);
}

function print_models_options($name, $myid, $readonly=false) {
	$query = "SELECT id, CONCAT(coalesce(model_name, ''), ' ', coalesce(revision, ')') AS name FROM models";
	print_select_options($name, $myid, $readonly, $query);
}

function print_citations_options($name, $myid, $readonly=false) {
	$query = "SELECT id, CONCAT(coalesce(author, ''), ' \"', coalesce(title, ''), '\" ') AS name FROM citations";
	print_select_options($name, $myid, $readonly, $query);
}

function print_species_options($name, $myid, $readonly=false) {
	$query = "SELECT id, scientificname AS name FROM species";
	if ($readonly) {
		print_select_options($name, $myid, $readonly, $query);
	} else {
		if ($myid == -1) {
			$values = array();
		} else {
			$values = array($myid => "Current value " . $myid);
		}
		print_select_array_options($name, $myid, $readonly, $values);
	}
}

function print_variables_options($name, $myid, $readonly=false) {
	$query = "SELECT id, name FROM variables";
	print_select_options($name, $myid, $readonly, $query);
}

function print_treatments_options($name, $myid, $readonly=false) {
	$query = "SELECT id, name FROM treatments";
	print_select_options($name, $myid, $readonly, $query);
}

function print_cultivars_options($name, $myid, $readonly=false) {
	$query = "SELECT id, name FROM cultivars";
	print_select_options($name, $myid, $readonly, $query);
}

function print_select_options($name, $myid, $readonly, $query) {
	global $pdo;

	if ($readonly) {
		if ($myid == "") {
			$query .= " WHERE id=-1";
		} else {
			$query .= " WHERE id=${myid}";	
		}
	}
	$result = $pdo->query($query . " ORDER BY name");
	if (!$result) {
		die('Invalid query "' . $query . '" : [' . error_database() . ']'  . error_database());
	}

	if ($readonly) {
		print "<select name=\"$name\" disabled>\n";
	} else {
		print "<select name=\"$name\">\n";
	}
	$html = "";
	$foundit = false;
	while($row = @$result->fetch(PDO::FETCH_ASSOC)) {
		$name = $row['name'];
		if ($name == "") {
			$name = "NO NAME {$row['id']}";
		} 
		if ($myid == $row['id']) {
			$html .= "<option value=\"{$row['id']}\" selected>$name</option>\n";
			$foundit = true;
		} else if (!$readonly) {
			$html .= "<option value=\"{$row['id']}\">$name</option>\n";
		}
	}
	if (! $foundit) {
		if (($myid == "") || ($myid == "-1")) {
			$html = "<option value=\"-1\" selected>Please make a selection</option>\n" . $html;
		} else {
			$html = "<option value=\"-1\" selected>No item with this id {$myid}</option>\n" . $html;
		}
	}
	print $html;
	print "</select>\n";

	$result->closeCursor();
}

function print_select_array_options($name, $myid, $readonly, $values) {	
	if ($readonly) {
		print "<select name=\"$name\" disabled>\n";
	} else {
		print "<select name=\"$name\">\n";
	}
	$html = "";
	$foundit = false;
	foreach ($values as $key => $val) {
		if ($myid == $key) {
			$html .= "<option value=\"{$key}\" selected>$val</option>\n";
			$foundit = true;
		} else if (!$readonly) {
			$html .= "<option value=\"{$key}\">$val</option>\n";
		}
	}
	if (! $foundit) {
		if (($myid == "") || ($myid == "-1")) {
			$html = "<option value=\"-1\" selected>Please make a selection</option>\n" . $html;
		} else {
			$html = "<option value=\"-1\" selected>No item with this id {$myid}</option>\n" . $html;
		}
	}
	print $html;
	print "</select>\n";
}

# ----------------------------------------------------------------------
# COMMON FUNCTIONS
# ----------------------------------------------------------------------
function starts_with($haystack, $needle) {
	return !strncmp($haystack, $needle, strlen($needle));
}
?>
