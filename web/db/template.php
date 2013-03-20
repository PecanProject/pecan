<?php
require("../common.php");

# what is the current id, table
if (isset($_REQUEST['id'])) {
	$id=$_REQUEST['id'];
} else {
	die('need an id');
}
preg_match("/^(.*)\/([a-zA-Z]*)\/([a-z]+)_([a-z]+).php$/",  $_SERVER['SCRIPT_NAME'], $matches);
$section = $matches[2];
$table = $matches[3];
$edit = ($matches[4] == "edit");

# Make sure we can get here.
if (($edit && (get_page_acccess_level() > 3)) || (get_page_acccess_level() > 4)) {
	header("Location: {$matches[1]}/index.php");
	die("Not authorized.");
}

# print top
print_header($section, $table);
print_menu($section);

# handle any information send in inputs form
$msg = "";
if ($edit && isset($_REQUEST['action'])) {
	if ($_REQUEST['action'] == "update") {
		$msg = editor_update($id, $table);
	}
}

# print navigation
print_prev_next($id, $table);

# print form to edit entry
print_editor($id, $table, !$edit);

# print footer of html
print_footer($msg);
?>
