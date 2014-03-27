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
if ($authentication) {
	open_database();
	if (!check_login()) {
		header( "Location: index.php");
		close_database();
		exit;
	}
	close_database();
}
?>
<!DOCTYPE html>
<html>
	<head>
		<title>The submission might have errors</title>
		<link rel="stylesheet" type="text/css" href="sites.css" />
		<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
		<script type="text/javascript">
			function prevStep() {
				$("#formprev").submit();
			}

			function nextStep() {
				$("#formnext").submit();
			}
		</script>
	</head>
	<body>
		<div id="wrap">
			<div id="stylized">
				<h1>Potential errors.</h1>
				<p>Click "Continue" if you wish to proceed to submit the run regardless or "Back" to change parameters and re-run.</p>

				<form id="formprev" method="POST" action="03-inputs.php">
					<?php foreach ($_REQUEST as $k => $v) {
						if (is_array($v)) {
							foreach($v as $x) {
								echo "<input type=\"hidden\" name=\"${k}[]\" value=\"${x}\" />\n";
							}
						} else {
							echo "<input type=\"hidden\" name=\"${k}\" value=\"${v}\" />\n";
						}
					} ?>
				</form>
				
				<form id="formnext" method="POST" action="05-running.php">
					<?php foreach ($_REQUEST as $k => $v) {
						if (is_array($v)) {
							foreach($v as $x) {
								echo "<input type=\"hidden\" name=\"${k}[]\" value=\"${x}\" />\n";
							}
						} else {
							echo "<input type=\"hidden\" name=\"${k}\" value=\"${v}\" />\n";
						}
					} ?>
				</form>

				<span id="error" class="small">&nbsp;</span>
				<input id="prev" type="button" value="Back" onclick="prevStep();" />
				<input id="next" type="button" value="Continue" onclick="nextStep();" />
				<div class="spacer"></div>
<?php
	if (check_login()) {
		echo "<p></p>";
		echo "Logged in as " . get_user_name();
		echo "<a href=\"index.php?logout\" id=\"logout\">logout</a>";
	}
?>		
			</div>
			<div id="output"><?php echo $_REQUEST['msg']; ?></div>
			<div id="footer">
				The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
				(ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
				Biosciences Institute</a>.
			</div>
		</div>
	<body>
<html>
