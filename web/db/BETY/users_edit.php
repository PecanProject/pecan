<?php
require("../common.php");

# what is the current id
if (isset($_REQUEST['id'])) {
	$id=$_REQUEST['id'];
} else {
	die('need an id');
}
$table = "users";

if ((get_page_acccess_level() > 1) && ($id != get_userid())) {
	header("Location: index.php");
}

print_header("BETY", $table);
print_menu("BETY");

# handle any information send in inputs form
$msg = "";
if (isset($_REQUEST['action'])) {
	if ($_REQUEST['action'] == "update") {
		$msg = editor_update($id, $table);
	}
}

if (get_acccess_level() == 1) {
	print_prev_next($id, $table);
}
print_editor($id, $table);
print_footer($msg);
?>
