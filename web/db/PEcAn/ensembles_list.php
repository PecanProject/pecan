<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "ensembles";
$query = "SELECT id, runtype, workflow_id FROM ensembles";

print_header("PEcAn", $table);
print_menu("PEcAn");
print_list($table, $query, $idkey);
print_footer();
?>
