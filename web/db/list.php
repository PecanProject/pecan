<?php
require "common.php";

# find the table
$table = isset($_REQUEST['table']) ? $_REQUEST['table'] : "";
$section = $sections[$table];
if (empty($section)) {
	#header("Location: index.php");
	die("Invalid table.");
}

# Make sure we can get here.
if (get_page_acccess_level() > $section['level']['show']) {
	#header("Location: index.php");
	die("Not authorized.");
}

# print top
print_header($table);
print_menu($section);

# create query to show things
$msg = print_list($table, $sections[$table]['list']);

# print footer of html
print_footer($msg);
?>
