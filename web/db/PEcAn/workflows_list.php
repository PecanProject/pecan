<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "workflows";
$query = "SELECT workflows.id as id, CONCAT(sitename, ', ', city, ', ', state, ', ', country) as site, models.model_name as model, workflows.started_at as 'start date', workflows.finished_at as 'end date' FROM workflows, sites, models WHERE workflows.site_id=sites.id AND workflows.model_id=models.id";

print_header("PEcAn", $table);
print_menu("PEcAn");
print_list($table, $query, $idkey);
print_footer();
?>
