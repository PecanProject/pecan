<?php
require("common.php");

print_header("Login");
print_menu("Login");

$msg="";
if (isset($_REQUEST['action'])) {
	if ($_REQUEST['action'] == "login") {
		if (login($_REQUEST['username'], $_REQUEST['password'])) {
			header("Location: index.php");
		}
		$msg="Invalid username and/or password.";
	}
}
?>

<form method="post" action="<?php echo $_SERVER['SCRIPT_NAME'] ?>">
	<div class="tbl" id="editor">
		<div class="row">
			<div class="key">Login</div>
			<div class="val"><input name="username" type="text"/></div>
		</div>
		<div class="row">
			<div class="key">Password</div>
			<div class="val"><input name="password" type="password"/></div>
		</div>
		<div class="row">
			<div class="key"><input name="action" type="submit" value="login"/></div>
			<div class="val"></div>
		</div>
	</div>
</form>

<?php
print_footer($msg);
?>
