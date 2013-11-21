<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "citations";
$query = "SELECT id, author, title FROM $table";

print_header("BETY", $table);
print_menu("BETY");
print_list($table, $query, $idkey);
print_footer();
?>
