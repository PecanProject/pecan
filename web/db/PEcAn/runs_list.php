<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "runs";
$query = "SELECT runs.id as id, CONCAT(sitename, ', ', city, ', ', state, ', ', country) AS site, CONCAT(model_name, ' r', revision) AS model, runs.started_at as 'start date', runs.finished_at as 'end date' FROM runs, sites, models WHERE runs.site_id=sites.id AND runs.model_id=models.id";

print_header("PEcAn", $table);
print_menu("PEcAn");
print_list($table, $query, $idkey);
print_footer();
?>
