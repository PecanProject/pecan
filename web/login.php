<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// Check login
require("common.php");
if (isset($_REQUEST['logout'])) {
	logout();
}
$error = "";

open_database();
if (check_login()) {
    if (get_page_acccess_level() <= $min_run_level) {
        header( "Location: 01-introduction.php");
    } else {
        header( "Location: history.php");
    }
    close_database();
    exit;
}
if (isset($_REQUEST['username']) && isset($_REQUEST['password'])) {
	if (login($_REQUEST['username'], $_REQUEST['password'])) {
        if (get_page_acccess_level() <= $min_run_level) {
            header( "Location: 01-introduction.php");
        } else {
            header( "Location: history.php");
        }
        close_database();
        exit;
	}
	$error = "Invalid username/password";
}
close_database();

?>
<!DOCTYPE html>
<html>
<head>
<title>EBI Sites</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
	function login() {
		if (($("#username")[0].value == "") || ($("#password")[0].value == "")) {
			$("#error").text("Please specify a username and password.");
		} else {
			$("#formnext").submit();
		}
	}
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form id="formnext" method="POST" action="login.php">
			<h1>Login</h1>

			<label>Username:</label>
			<input name="username" id="username" style="align: left" type="text">
			<div class="spacer"></div>

			<label>Password:</label>
			<input name="password" id="password" style="align: left" type="password">
			<div class="spacer"></div>

			<p></p>
			<span id="error" class="small"><?php echo $error; ?></span>
			<input id="next" type="button" value="Login" onclick="login();" />
			
			<div class="spacer"></div>
		</form>
	</div>
	<div id="output">
		<h1>Login</h1>
		<p>This installation of PEcAn requires a username and password. You can use the same username/password as used for BETY.</p>
	</div>
	<div id="footer">
		The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
		(ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
		Biosciences Institute</a>.
	</div>
</div>
</body>
</html>
