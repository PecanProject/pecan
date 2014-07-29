<?php
require "common.php";

# find the table
$table = isset($_REQUEST['table']) ? $_REQUEST['table'] : "";
$section = $sections[$table];
if (empty($section)) {
	#header("Location: index.php");
	die("Invalid table.");
}

# what is the current id
if (isset($_REQUEST['id'])) {
	$id=$_REQUEST['id'];
} else {
	#header("Location: index.php");
	die('need an id');
}

# Make sure we can get here.
if ($sections[$table]['level']['show'] < get_page_acccess_level() && ($table != "users" || $id != get_userid())) {
	#header("Location: index.php");
	die("Not authorized.");
}

# print top
print_header($table);
print_menu($section);

# print form to show entry
print_entry($id, $table, true);

# list files associated
if ($section['files']) {
	show_files($id, $table, true);	
}

# print footer of html
print_footer();
?>
