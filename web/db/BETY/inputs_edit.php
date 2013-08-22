<?php
require("../common.php");

if (get_page_acccess_level() > 3) {
	header("Location: index.php");
}

# what is the current inputs id
if (isset($_REQUEST['id'])) {
	$id=$_REQUEST['id'];
} else {
	die('need an id');
}

$table = "inputs";

print_header("BETY", $table);
print_menu("BETY");


# handle any information send in inputs form
$msg = "";
if (isset($_REQUEST['action'])) {
	if ($_REQUEST['action'] == "update") {
		$msg = editor_update($id, $table);
	}

	if ($_REQUEST['action'] == "add") {
		$query = "UPDATE dbfiles, inputs SET dbfiles.container_id=coalesce(inputs.file_id, inputs.id), inputs.file_id=coalesce(inputs.file_id, inputs.id) WHERE dbfiles.id={$_REQUEST['dbid']} AND inputs.id=${id};";
		if (!mysql_query($query, $db_connection)) {
			$msg = "Error updating database : [" . mysql_errno($db_connection) . "] " . mysql_error($db_connection) . "<br>";
			editor_log("FAIL", $query);
		} else {
			$msg .= "Added dbfiles={$_REQUEST['dbid']} to inputs={$_REQUEST['id']}<br/>\n";
			editor_log("OK", $query);
		}
	}

	if ($_REQUEST['action'] == "del") {
		$query = "UPDATE dbfiles SET container_id=NULL, container_type=NULL WHERE id={$_REQUEST['dbid']};";
		if (!mysql_query($query, $db_connection)) {
			$msg = "Error updating database : [" . mysql_errno($db_connection) . "] " . mysql_error($db_connection) . "<br>";
			editor_log("FAIL", $query);
		} else {
			$msg .= "Removed dbfiles={$_REQUEST['dbid']} from inputs={$_REQUEST['id']}<br/>\n";
			editor_log("OK", $query);
		}
	}
}

print_prev_next($id, $table);
print_editor($id, $table);

if ($id != -1) {
?>
	<script type="text/javascript">
		function add_input(id) {
			var e = document.getElementById(id);
			var v = e.options[e.selectedIndex].value;
			window.location.href = "inputs_edit.php?id=<?php echo $id ?>&action=add&dbid=" + v;
		}
		function del_input(id) {
			var e = document.getElementById(id);
			var v = e.options[e.selectedIndex].value;
			window.location.href = "inputs_edit.php?id=<?php echo $id ?>&action=del&dbid=" + v;
		}
		function show_input(id) {
			var e = document.getElementById(id);
			var v = e.options[e.selectedIndex].value;
			window.location.href = "dbfiles_show.php?id=" + v;
		}
		function edit_input(id) {
			var e = document.getElementById(id);
			var v = e.options[e.selectedIndex].value;
			window.location.href = "dbfiles_edit.php?id=" + v;
		}
	</script> 

	<hr />
	Existing Files<br/>
<?php
	# get the input that we want to show
	$query="SELECT dbfiles.id, concat(machines.hostname, ':', dbfiles.file_path, '/', dbfiles.file_name) as filename" .
	       " FROM dbfiles, machines, inputs" .
	       " WHERE inputs.id=$id AND dbfiles.container_id=inputs.file_id AND machines.id=dbfiles.machine_id;";
	$result = mysql_query($query, $db_connection);
	if (!$result) {
		die("Invalid query [$query] " . mysql_error($db_connection));
	}
	if (mysql_num_rows($result) > 0) {
?>
	<div class="tbl" id="editor">
		<div class="row">
			<div class="key">
				<a href="javascript:void(0)" title="Show DBFile" onclick="show_input('delfile')">S</a>
				<a href="javascript:void(0)" title="Edit DBFile" onclick="edit_input('delfile')">E</a>
				<a href="javascript:void(0)" title="Remove DBFile from Input" onclick="del_input('delfile')">D</a>
			</div>
			<div class="val">
				<select id="delfile">
<?php
		while($dbfilerow = @mysql_fetch_assoc($result)) {
			if (isset($_REQUEST['dbid']) && ($_REQUEST['dbid'] == $dbfilerow['id'])) {
				print "<option value=\"{$dbfilerow['id']}\" selected>{$dbfilerow['filename']}</option>\n";
			} else {
				print "<option value=\"{$dbfilerow['id']}\">{$dbfilerow['filename']}</option>\n";
			}
		}
		mysql_free_result($result);
?>
				</select>
			</div>
		</div>
	</div>
<?php
	}
?>
	<hr />
	Add a Files<br/>
<?php
	$query="SELECT dbfiles.id, concat(machines.hostname, ':', dbfiles.file_path, '/', dbfiles.file_name) as filename" .
	       " FROM dbfiles, machines" .
	       " WHERE machines.id = dbfiles.machine_id AND NOT EXISTS ( SELECT 1 FROM inputs WHERE dbfiles.container_id=inputs.file_id ) " .
	       " ORDER BY filename;";
	$result = mysql_query($query, $db_connection);
	if (!$result) {
		die("Invalid query [$query] " . mysql_error());
	}
	if (mysql_num_rows($result) > 0) {
?>
	<div class="tbl" id="editor">
		<div class="row">
			<div class="key">
				<a href="javascript:void(0)" title="Show DBFile" onclick="show_input('addfile')">S</a>
				<a href="javascript:void(0)" title="Edit DBFile" onclick="edit_input('addfile')">E</a>
				<a href="javascript:void(0)" title="Add DBFile to Input" onclick="add_input('addfile')">A</a>
			</div>
			<div class="val">
				<select id="addfile">
<?php				
		while($dbfilerow = @mysql_fetch_assoc($result)) {
			if ($dbfilerow['filename'] == "") {
				continue;
			}
			if (isset($_REQUEST['dbid']) && ($_REQUEST['dbid'] == $dbfilerow['id'])) {
				print "<option value=\"{$dbfilerow['id']}\" selected>{$dbfilerow['filename']}</option>\n";
			} else {
				print "<option value=\"{$dbfilerow['id']}\">{$dbfilerow['filename']}</option>\n";
			}
		}
		mysql_free_result($result);
?>
				</select>
			</div>
		</div>
	</div>
<?php
	}
}
print_footer($msg);
?>
