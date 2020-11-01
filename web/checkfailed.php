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
    if (get_page_acccess_level() > $min_run_level) {
       header( "Location: history.php");
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
		<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
		<link rel="stylesheet" type="text/css" href="sites.css" />
		<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
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
<?php whoami(); ?>    
<p>
  <a href="https://pecan.gitbooks.io/pecan-documentation/content/" target="_blank">Documentation</a>
  <br>
  <a href="https://join.slack.com/t/pecanproject/shared_invite/enQtMzkyODUyMjQyNTgzLWEzOTM1ZjhmYWUxNzYwYzkxMWVlODAyZWQwYjliYzA0MDA0MjE4YmMyOTFhMjYyMjYzN2FjODE4N2Y4YWFhZmQ" target="_blank">Chat Room</a>
  <br>
  <a href="http://pecanproject.github.io/Report_an_issue.html" target="_blank">Bug Report</a>
</p>
			</div>
			<div id="output"><?php echo $_REQUEST['msg']; ?></div>
			<div id="footer"><?php echo get_footer(); ?></div>
		</div>
	<body>
<html>
