<?php

// runid
if (!isset($_REQUEST['runid'])) {
  die("Need a runid.");
}
$runid=$_REQUEST['runid'];

// database parameters
require("dbinfo.php");

// Opens a connection to a MySQL server
$connection=mysql_connect ($hostname, $username, $password);
if (!$connection) {
	die('Not connected : ' . mysql_error());
}

// Set the active MySQL database
$db_selected = mysql_select_db($database, $connection);
if (!$db_selected) {
	die ('Can\'t use db : ' . mysql_error());
}

// get run information
$query = "SELECT start_time, finish_time, outdir FROM runs WHERE runs.id=$runid";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$run = mysql_fetch_assoc($result);
$start = substr($run['start_time'], 0, 4);
$end = substr($run['finish_time'], 0, 4);
$folder = $run['outdir'];

$years="";

$vars  = "";
$vars .= "<option>Reco</option>\n";
$vars .= "<option>NPP</option>\n";
$vars .= "<option>NEE</option>\n";

$outputs  = "";
$outputs .= "<option>pecan.xml</option>";

$files=scandir("$folder/out");
for($year=$start; $year<=$end; $year++) {
	$years .= "<option>$year</option>";
	
	// tower file
	$tower = array_pop(array_filter($files, function ($item) {
		global $year;
		return preg_match("/.*-T-$year-00-00-000000-g01.h5/", $item);
	}));
	$outputs .= createOption("out/$tower");
	
	// get variables
	$vars .= shell_exec("h5ls $folder/out/$tower | awk '{print \"<option>\" $1 \"</option>\" }'");	
}

$logs="";
$logs .= createOption("setup.Rout");
if (file_exists("$folder/fia2ED.Rout")) {
	$logs .= createOption("fia2ED.Rout");
}
$logs .= createOption("query.bety.Rout");
$logs .= createOption("meta.analysis.Rout");
$logs .= createOption("write.configs.Rout");
foreach(scandir("$folder/run") as $file) {
	if (substr($file, -4) === ".log") {
		$logs .= createOption("run/$file");
	}
}
$logs .= createOption("plots.Rout");
$logs .= createOption("finished.Rout");

function createOption($file) {
	$ext = strrchr($file, ".");
	$name = basename($file, $ext);
	return "<option value=\"$file\">$name</option>\n";
}
?>
<!DOCTYPE html>
<html>
<head>
<title>EBI Results</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script type="text/javascript">
	google.load("jquery", "1.3.2");

	function resize() {
		$("#stylized").height($(window).height() - 5);
		$("#output").height($(window).height() - 1);
		$("#output").width($(window).width() - $('#form').width() - 15);
	} 

    function showPlot() {
		var url="dataset.php?runid=<?=$runid?>&type=plot&year=" + $('#year')[0].value + "&var=" + $('#var')[0].value;
		$("#output").html("<img src=\"" + url + "\">");
	}
	
	function showLog() {
		var url="dataset.php?runid=<?=$runid?>&type=file&name=" + $('#log')[0].value;
		jQuery.get(url, {}, setOuput);
	}

	function showFile() {
		var url="dataset.php?runid=<?=$runid?>&type=file&name=" + $('#outputs')[0].value;
		if (endsWith(url, ".xml")) {
			jQuery.get(url, {}, function(data) {
				setOuput((new XMLSerializer()).serializeToString(data));
			});
		} else {
			window.location = url;
		}
	}

	function setOuput(data) {
		data = data.replace(/&/g, "&amp;").replace(/</g,"&lt;").replace(/>/g, "&gt;").replace(/\"/g, "&quot;");
		$("#output").html("<pre>" + data + "</pre>");
	}

	function endsWith(haystack, needle) {
		return (haystack.substr(haystack.length - needle.length) === needle);
	}
	
    window.onresize = resize;
    window.onload = resize;
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form action="#" id="form">
			<h1>Plots</h1>
			<p>Results from PEcAn.</p>
			
			<h2>Plots</h2>
			<label>Selected Year</label>
			<select id="year">
				<?=$years?>
			</select>
			<div class="spacer"></div>
			<label>Selected Variable</label>
			<select id="var">
				<?=$vars?>
			</select>
			<div class="spacer"></div>
			<input id="home" type="button" value="Show Plot" onclick="showPlot();" />
			<div class="spacer"></div>

			<h2>Output Files</h2>
			<select id="outputs">
				<?=$outputs?>
			</select>
			<div class="spacer"></div>
			<input id="home" type="button" value="Download File" onclick="showFile();" />
			<div class="spacer"></div>

			<h2>Log Files</h2>
			<select id="log">
				<?=$logs?>
			</select>
			<div class="spacer"></div>
			<input id="home" type="button" value="Show Log" onclick="showLog();" />
			<div class="spacer"></div>
		</form>
	</div>
	<div id="output">Please select an option on the left.</div>
</div>
</body>
</html>
