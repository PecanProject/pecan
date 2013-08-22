<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

$idkey = "id";
$table = "dbfiles";
$query = "SELECT dbfiles.id as id, machines.hostname as machine, dbfiles.file_path as filepath, dbfiles.file_name as filename FROM dbfiles, machines WHERE dbfiles.machine_id=machines.id";

print_header("BETY", "FILES");
print_menu("BETY");
print_list($table, $query, $idkey);
print_footer();
?>
