<!DOCTYPE html>
<html>
	<head>
		<title>The submission might have errors</title>
		<link rel="stylesheet" type="text/css" href="sites.css" />
		<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
		<script type="text/javascript">
			window.onresize = resize;
			window.onload = resize;
			
	        function resize() {
	                if ($("#stylized").height() < $(window).height()) {
	                        $("#stylized").height($(window).height() - 5);
	                }
	                $("#output").height($(window).height() - 1);
	                $("#output").width($(window).width() - $('#stylized').width() - 5);
	        }

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

				<form id="formprev" method="POST" action="selectdata.php">
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
				
				<form id="formnext" method="POST" action="runpecahn.php">
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
			</div>
			<div id="output"><?= $_REQUEST['msg'] ?></div>
		</div>
	<body>
<html>
