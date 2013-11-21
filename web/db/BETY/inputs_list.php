<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}


$idkey = "id";
$table = "inputs";
$query = "SELECT $table.id AS id, inputs.name AS name, sites.sitename AS sitename, count(dbfiles.id) AS files" .
         " FROM $table".
         " LEFT JOIN sites ON sites.id = $table.site_id" .
         " LEFT JOIN dbfiles ON dbfiles.container_id = $table.id AND dbfiles.container_type='Input'" . 
         " GROUP BY id";

print_header("BETY", "INPUTS");
print_menu("BETY");
print_list($table, $query, $idkey);
print_footer();
?>
