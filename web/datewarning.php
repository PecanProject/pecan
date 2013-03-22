<!DOCTYPE html>
<html>
	<head>
		<title>Date warning and confirmation</title>
		<link rel="stylesheet" type="text/css" href="sites.css" />
		<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
		<?php 
			# offline mode?
			if (isset($_REQUEST['offline'])) {
				$offline=true;
			} else {
				$offline=false;
			}
			
			if (!$offline) {
		?>
				<script type="text/javascript" src="http://www.google.com/jsapi"></script>
		<?php } ?>
		<script type="text/javascript">
			window.onresize = resize;
			window.onload = resize;
			
			function resize() {
				$("#stylized").height($(window).height() - 5);
				$("#map_canvas").height($(window).height() - 1);
				$("#map_canvas").width($(window).width() - $('#stylized').width() - 40);
				document.getElementById('map_canvas').style.paddingLeft = "25px";
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
		<?php 
			# parameters
			$startdate=$_REQUEST['start'];
			$enddate=$_REQUEST['end'];
			$siteid=$_REQUEST['siteid'];
			$modelid=$_REQUEST['modelid'];
			$modeltype=$_REQUEST['modeltype'];
			$hostname=$_REQUEST['hostname'];
			$pft=$_REQUEST['pft'];
			$psscss=$_REQUEST['psscss'];
			$met=$_REQUEST['met'];
			$advanced_edit=(isset($_REQUEST['advanced_edit']) && ($_REQUEST['advanced_edit'] != FALSE)); //Advanced edit will always exist and be false if not set
		?>

		<div id="wrap">
			<div id="stylized">

				<form id="formprev" method="POST" action="selectdata.php">
					<?php if ($offline) { ?>
						<input name="offline" type="hidden" value="offline">
					<?php } ?>
					<input type="hidden" name="start" value="<?=$startdate?>" />
					<input type="hidden" name="end" value="<?=$enddate?>" />
					<input type="hidden" name="siteid" value="<?=$siteid?>" />
					<input type="hidden" name="psscss" value="<?=$psscss?>" />
					<input type="hidden" name="met" value="<?=$met?>" />
					<input type="hidden" name="modelid" value="<?=$modelid?>" />
					<input type="hidden" name="modeltype" value="<?=$modeltype?>" />
					<input type="hidden" name="hostname" value="<?=$hostname?>" />
					<?php foreach ($pft as $pft_iter) { ?>
					<input type="hidden" name="pft[]" value="<?=$pft_iter?>" />
					<?php } 
					//unset($pft_iter); //not actually necessary as it would just be overwritten if we return
					?>
					<input type="hidden" name="advanced_edit" value="<?=$advanced_edit?>" /> 
				</form>
				<form id="formnext" method="POST" action="runpecan.php">
					<?php if ($offline) { ?>
						<input name="offline" type="hidden" value="offline">
					<?php } ?>
					<input type="hidden" name="start" value="<?=$startdate?>" />
					<input type="hidden" name="end" value="<?=$enddate?>" />
					<input type="hidden" name="siteid" value="<?=$siteid?>" />
					<input type="hidden" name="modelid" value="<?=$modelid?>" />
					<input type="hidden" name="modeltype" value="<?=$modeltype?>" />
					<input type="hidden" name="hostname" value="<?=$hostname?>" />
					<input type="hidden" name="psscss" value="<?=$psscss?>" />
					<input type="hidden" name="met" value="<?=$met?>" />
					<?php foreach ($pft as $pft_iter) { ?>
					<input type="hidden" name="pft[]" value="<?=$pft_iter?>" />
					<?php } ?>
					<input type="hidden" name="advanced_edit" value="<?=$advanced_edit?>" /> 
					<input type="hidden" name="user_ok" value="yes" /> 

					<span id="error" class="small"></span>
					<input id="prev" type="button" value="Back" onclick="prevStep();" />
					<input id="next" type="button" value="Continue" onclick="nextStep();" />		
				</form>
			</div>
			<div id="map_canvas">
				<h1>Warning</h1>
				<label>Problem with dates</label>
				<p><h4>The selected dates are not within the bounds of the weather data file you selected. This can cause unpredictable behavior in some models.</h4></p> 
				<br>
				<p>Click "Continue" if you wish to proceed anyway or "Back" to change parameters.</p>
			</div>
		</div>
	<body>
<html>
