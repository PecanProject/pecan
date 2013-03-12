<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "sites";
$query = "SELECT id, sitename, city, state, country FROM sites";

print_header("BETY", $table);
print_menu("BETY");
print_list($table, $query, $idkey);
print_footer();
?>
