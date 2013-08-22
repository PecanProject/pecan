<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "models";
$query = "SELECT id, model_name AS name, revision, model_path AS filepath FROM models";

print_header("PEcAn", $table);
print_menu("PEcAn");
print_list($table, $query, $idkey);
print_footer();
?>
