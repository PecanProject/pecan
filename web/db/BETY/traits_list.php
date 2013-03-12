<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "traits";
$query = "SELECT $table.id AS id, sites.sitename AS sitename, species.commonname AS speciename, statname" .
         " FROM $table".
         " LEFT JOIN sites ON sites.id = $table.site_id" .
         " LEFT JOIN species ON species.id = $table.specie_id";


print_header("BETY", $table);
print_menu("BETY");
print_list($table, $query, $idkey);
print_footer();
?>
