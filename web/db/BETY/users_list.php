<?php
require("../common.php");

$idkey = "id";
$table = "users";
$query = "SELECT id, login, name, email FROM users";

if (get_page_acccess_level() > 1) {
	$query = "$query WHERE id=" . get_userid();
}

print_header("BETY", "USERS");
print_menu("BETY");
$msg = print_list($table, $query, $idkey);
print_footer($msg);
?>
