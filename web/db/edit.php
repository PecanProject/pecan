<?php
require "common.php";

# find the table
$table = isset($_REQUEST['table']) ? $_REQUEST['table'] : "";
$section = $sections[$table];
if (empty($section)) {
	header("Location: index.php");
	die("Invalid table.");
}

# what is the current id
if (isset($_REQUEST['id'])) {
	$id=$_REQUEST['id'];
} else {
	header("Location: index.php");
	die('need an id');
}

# Make sure we can get here.
if ($sections[$table]['level']['edit'] < get_page_acccess_level() && ($table != "users" || $id != get_userid())) {
	#header("Location: {$matches[1]}/index.php");
	die("Not authorized. " . get_userid());
}

# print top
print_header($table);
print_menu($section);

# print form to edit entry
$msg = print_entry($id, $table, false);

# list files associated
if ($section['files']) {
	$tmp = show_files($id, $table, false);
	if ($msg == "") {
		$msg = $tmp;
	} else if ($tmp != "") {
		$msg = "{$msg}<br/>{$tmp}";
	}
}

# print footer of html
print_footer($msg);
?>
