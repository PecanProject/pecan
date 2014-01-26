<?php

require("config.php");

# Single share connection
$db_connection=null;

# sections to show in menu (subfolders)
$sections=array("BETY", "PEcAn");

# make sure we do a session start
session_start();

# ----------------------------------------------------------------------
# DATABASE FUNCTIONS
# ----------------------------------------------------------------------
function open_database() {
	global $db_hostname;
	global $db_username;
	global $db_password;
	global $db_database;
	global $db_type;
	global $db_connection;

	$db_connection = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", ${db_username}, ${db_password});
}

function close_database() {
	global $db_connection;
	$db_connection = null;
}

# ----------------------------------------------------------------------
# COMMON HTML FUNCTIONS
# ----------------------------------------------------------------------
function print_header($section, $table="") {
	global $sections;

	open_database();

	print "<html>\n";
	print "<head>\n";
	if ($table == "") {
		print "<title>BETY - $section</title>\n";
	} else {
		print "<title>$section - $table</title>\n";
	}
	if (in_array($section, $sections)) {
		print "<link href=\"../bety.css\" rel=\"stylesheet\" type=\"text/css\"/>\n";
	} else {
		print "<link href=\"bety.css\" rel=\"stylesheet\" type=\"text/css\"/>\n";
	}
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

	if (in_array($active, $sections)) {
		$folder="../";
	} else {
		$folder="";
	}
	$menu=array("Home" => "${folder}index.php");

	foreach($sections as $section) {
		$menu[$section] = array();

		if (get_page_acccess_level() <= 4) {
			if ($handle = opendir("${folder}${section}")) {
				while (false !== ($entry = readdir($handle))) {
					if (($pos = stripos($entry, "_list.php"))) {
						$x = ucwords(substr($entry, 0, $pos));
						$menu[$section][$x]["#"] = "${folder}${section}/${entry}";
					}
					if ((get_page_acccess_level() <= 3) && ($pos = stripos($entry, "_edit.php"))) {
						$x = ucwords(substr($entry, 0, $pos));
						if ($x != "Users" || get_page_acccess_level() == 1) {
							$menu[$section][$x]["New"] = "${folder}${section}/${entry}?id=-1";
						}
					}

				}
				closedir($handle);
			}
		}
		ksort($menu[$section]);	
	}

	if (check_login()) {
		$menu[get_user_name()] = array(
			"Edit" => "${folder}BETY/users_edit.php?id=" . get_userid(),
			"Logout" => "${folder}logout.php",
			);
	} else {
		$menu["Login"] = "${folder}login.php";
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
	global $db_connection;

	if (isset($_SESSION['userid'])) {
		return TRUE;
	}

	$result = $db_connection->query("SELECT * FROM users WHERE login='" . $db_connection->quote($username) . "'", $db_connection);
	if (!$result) {
		die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}
	$row = $result->fetch(PDO::FETCH_ASSOC);
	$result->closeCursor();

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

function print_list($table, $query, $idkey) {
	global $pagesize;
	global $db_connection;

	# handle any information send in inputs form
	$msg = "";
	if (isset($_REQUEST['action'])) {
		if ($_REQUEST['action'] == "delete") {
			if (($table != "users") || ((get_page_acccess_level() == 1) && ($_REQUEST['kill'] != get_userid()))) {
				if ($db_connection->query("DELETE FROM $table WHERE ${idkey}={$_REQUEST['kill']};", $db_connection)) {
					$msg = "Removed {$_REQUEST['kill']} from {$table}";
				} else {
					$msg = "Error updating database : [" . $db_connection->errorInfo() . "] " . $db_connection->errorInfo($db_connection) . "<br>$query";
				}
			}
		}
	}

	if (isset($_REQUEST['pagesize'])) {
		$pagesize = $_REQUEST['pagesize'];
	}

	# fix access_level
	if (($table != "users") && (get_page_acccess_level() > 1)) {
		$result = $db_connection->query("SHOW COLUMNS FROM $table LIKE 'access_level';", $db_connection);
		if (!$result) {
			die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
		}
		if ($result->fetchColumn() > 0) {
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
	$result = $db_connection->query($query . " ORDER BY $idkey LIMIT $pagesize OFFSET " . (($current - 1) * $pagesize), $db_connection);
	if (!$result) {
		die("Invalid query : $query [" . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}

	$header = false;
	while($row = @$result->fetch(PDO::FETCH_ASSOC)) {
		if (!$header) {
			print "<div class=\"tbl\" id=\"list\">\n";
			print "    <div class=\"row\">\n";
			print "      <div class=\"hdr id\">Action</div>\n";
			if (array_key_exists($idkey, $row)) {
				print "      <div class=\"hdr id\">id</div>\n";
			}
			foreach ($row as $key => $value) {
				if ($key != $idkey) {
					print "      <div class=\"hdr $key\">$key</div>\n";
				}
			}
			print "    </div>\n";
			$header = true;
		}

		print "    <div class=\"row\">\n";
		if (array_key_exists($idkey, $row)) {
			print "      <div class=\"col id\">";
			if (file_exists("{$table}_show.php")) {
				print "<a title=\"Show {$row[$idkey]} in {$table}\" href=\"{$table}_show.php?id={$row[$idkey]}\">S</a> ";
			}
			if ((get_page_acccess_level() <= 3) && file_exists("{$table}_edit.php")) {
				print "<a title=\"Edit {$row[$idkey]} in {$table}\"  href=\"{$table}_edit.php?id={$row[$idkey]}\">E</a> ";
			}
			if ((get_page_acccess_level() <= 3) && (($table != "users") || ($row[$idkey] != get_userid()))) {
				$url="{$table}_list.php?page={$current}&action=delete&kill={$row[$idkey]}";
				print "<a title=\"Remove {$row[$idkey]} frome {$table}\"  href=\"$url\" onclick=\"return confirm('Are you sure you want to delete item {$row[$idkey]}?')\">D</a> ";
			}
			print "</div>\n";
			print "      <div class=\"col id $idkey\">{$row[$idkey]}</div>\n";
		}
		foreach ($row as $key => $value) {
			if ($key != $idkey) {
				print "      <div class=\"col $key\">$value</div>\n";
			}
		}
		print "  </div>\n";
	}
	if ($header) {
		print "</div>\n";
	}
	$result->closeCursor();

	print_pages($current, $pagesize, $query, $table);

	return $msg;
}

function print_pages($current, $pagesize, $query, $table) { 
	global $db_connection;

	# count items
	$result = $db_connection->query("$query", $db_connection);
	if (!$result) {
		die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}
	$count = $result->fetchColumn();
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
# EDIT PAGE FUNCTIONS
# ----------------------------------------------------------------------

function editor_log($status, $query) {
	global $logfile;
	if (is_writeable($logfile)) {
		file_put_contents($logfile, date("c") . "\t${status}\t" . get_userid() . "\t" . get_user_name() . "\t${query}\n", FILE_APPEND);
	}
}

function editor_update($id, $table) {
	global $db_connection;

	if (($table == "users") && (($id != get_userid()) || (get_page_acccess_level() != 1))) {
		header("Location: ../index.php");
		return;
	}
	if (get_page_acccess_level() > 3) {
		header("Location: ../index.php");
		return;
	}

	# get the row from the database (this can be empty)
	$result = $db_connection->query("SELECT * FROM $table WHERE id=$id;", $db_connection);
	if (!$result) {
		die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}
	$row = $result->fetch(PDO::FETCH_ASSOC);
	$result->closeCursor();

	$msg = "";
	$set = "";
	foreach($_REQUEST as $key => $val) {
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
				$set .= " $key='" . $db_connection->quote($val) . "'";
			}
		} else if ($pre == 'n') {
			if ($set != "") {
				$set .= ", ";
			}
			$set .= " $key=" . $db_connection->quote($val);
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
				$set .= " $key='" . $db_connection->quote($val) . "'";
			}
		}
	}
	if ($set != "") {
		if ($id == "-1") {
			$query = "INSERT INTO $table SET $set;";
			$db_connection->query($query, $db_connection);
			$id = $db_connection->lastInsertId();
			if (!$db_connection->query($query, $db_connection)) {
				$msg = "Error updating database : [" . $db_connection->errorInfo() . "] " . $db_connection->errorInfo($db_connection) . "<br>";
				editor_log("FAIL", $query);
			} else {
				$msg .= "Added into $table table id=$id<br/>\n";
				editor_log("OK", $query);
			}
		} else {
			$query = "UPDATE $table SET $set WHERE id=$id;";
			if (!$db_connection->query($query, $db_connection)) {
				$msg = "Error updating database : [" . $db_connection->errorInfo() . "] " . $db_connection->errorInfo($db_connection) . "<br>";
				editor_log("FAIL", $query);
			} else {
				$msg .= "Updated $table table for id=$id<br/>\n";
				editor_log("OK", $query);
			}
		}
	} else {
		if ($id == "-1") {
			$result = $db_connection->query("SELECT id FROM $table ORDER BY id ASC LIMIT 1;", $db_connection);
			if (!$result) {
				die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
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

function print_editor($id, $table, $readonly=false) {
	global $db_connection;

	if (($table == "users") && ($id != get_userid()) && (get_page_acccess_level() != 1)) {
		header("Location: ../index.php");
		return;
	}
	if (!$readonly && (get_page_acccess_level() > 3)) {
		header("Location: ../index.php");
		return;
	}
	if ($readonly && (get_page_acccess_level() > 4)) {
		header("Location: ../index.php");
		return;
	}

	if ($readonly) {
		$disabled = "disabled";
	} else {
		$disabled = "";
	}

	# get the row from the database (this can be empty)
	$result = $db_connection->query("SELECT * FROM $table WHERE id=$id;", $db_connection);
	if (!$result) {
		die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}
	$row = $result->fetch(PDO::FETCH_ASSOC);
	$result->closeCursor();

	# check access_level
	if (is_array($row) && array_key_exists('access_level', $row) && ($row['access_level'] != "") && ($row['access_level'] != "-1")) {
		if (get_acccess_level() > $row['access_level']) {
			header("Location: ../index.php");
			return;
		}
	}

	# get table structure
	$result = $db_connection->query("SHOW COLUMNS FROM $table;", $db_connection);
	if (!$result) {
		die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}

	if (!$readonly) {
		print "<form method=\"post\" action=\"{$_SERVER['SCRIPT_NAME']}\">\n";
		print "<input name=\"id\" type=\"hidden\" value=\"$id\"/>\n";
	}
	print "<div class=\"tbl\" id=\"editor\">\n";

	while($fields = $result->fetch(PDO::FETCH_ASSOC)) {
		$key = $fields['Field'];
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
			if (get_acccess_level() == 1) {
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
			if ((get_acccess_level() == 1) || ($val == "") || ($val == "-1")) {
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
		} else if (stristr($fields['Type'], "text")) {
			print "<div class=\"key\">$fancykey</div>\n";
			print "<div class=\"val\"><textarea name=\"s_$key\" rows=\"10\" $disabled>$val</textarea></div>\n";
		} else if (stristr($fields['Type'], "tinyint")) {			
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
}

function print_prev_next($id, $table) {
	global $db_connection;

	$and = "";
	$where = "";
	if (get_page_acccess_level() > 1) {
		$result = $db_connection->query("SHOW COLUMNS FROM $table LIKE 'access_level';", $db_connection);
		if (!$result) {
			die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
		}
		if ($result->fetchColumn() > 0) {
			$and   = " AND (access_level >= " . get_acccess_level() . " OR access_level IS NULL)";
			$where = " WHERE (access_level >= " . get_acccess_level() . " OR access_level IS NULL)";
		}
	}

	$result = $db_connection->query("SELECT id FROM {$table} WHERE id < ${id} ${and} ORDER BY id DESC LIMIT 1;", $db_connection);
	if (!$result) {
		die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}
	$row = $result->fetch(PDO::FETCH_NUM);
	$prev = $row[0];
	$result->closeCursor();
	if ($prev == "") {
		$result = $db_connection->query("SELECT id FROM {$table} ${where} ORDER BY id DESC LIMIT 1;", $db_connection);
		if (!$result) {
			die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
		}
		$row = $result->fetch(PDO::FETCH_NUM);
		$prev = $row[0];
		$result->closeCursor();
	}
	print "<div style=\"float: left\">";
	print "<a href=\"{$_SERVER['SCRIPT_NAME']}?id={$prev}\">&lt;Prev {$table} [{$prev}]&gt;</a>";
	print "</div>\n";

	$result = $db_connection->query("SELECT id FROM $table WHERE id > ${id} ${and} ORDER BY id ASC LIMIT 1;", $db_connection);
	if (!$result) {
		die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
	}
	$row = $result->fetch(PDO::FETCH_NUM);
	$next = $row[0];
	$result->closeCursor();
	if ($next == "") {
		$result = $db_connection->query("SELECT id FROM $table ${where} ORDER BY id ASC LIMIT 1;", $db_connection);
		if (!$result) {
			die('Invalid query : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
		}
		$row = $result->fetch(PDO::FETCH_NUM);
		$next = $row[0];
		$result->closeCursor();
	}
	print "<div style=\"float: right\">";
	print "<a href=\"{$_SERVER['SCRIPT_NAME']}?id={$next}\">&lt;Next {$table} [{$next}]&gt;</a>";
	print "</div>\n";
	print "<br/><br/>\n";
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
	$query = "SELECT id, CONCAT(coalesce(model_name, ''), ' r', coalesce(revision, ''), ' (', coalesce(model_path, ''), ')') AS name FROM models";
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
	global $db_connection;

	if ($readonly) {
		if ($myid == "") {
			$query .= " WHERE id=-1";
		} else {
			$query .= " WHERE id=${myid}";	
		}
	}
	$result = $db_connection->query($query . " ORDER BY name", $db_connection);
	if (!$result) {
		die('Invalid query "' . $query . '" : [' . $db_connection->errorInfo() . ']'  . $db_connection->errorInfo($db_connection));
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
