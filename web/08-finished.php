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
open_database();
if ($authentication) {
	if (!check_login()) {
		header( "Location: index.php");
		close_database();
		exit;
	}
}

# boolean parameters
$offline=isset($_REQUEST['offline']);

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// database parameters
require("system.php");
$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", $db_username, $db_password);

// get run information
$query = "SELECT * FROM workflows WHERE workflows.id=$workflowid";
$result = $pdo->query($query);
if (!$result) {
	die('Invalid query: ' . error_database());
}
$workflow = $result->fetch(PDO::FETCH_ASSOC);
$start = substr($workflow['start_date'], 0, 4);
$end = substr($workflow['end_date'], 0, 4);
$folder = $workflow['folder'];

# check to make sure all is ok
$error=false;
$status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
if ($status === FALSE) {
	$status = array();
	$error = true;
}
foreach ($status as $line) {
	$data = explode("\t", $line);
	if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
		$error = true;
	}
}

# check the PEcAn folder
$logs="";
foreach(scandir("$folder") as $file) {
	if (is_dir("$folder/$file") || ($file == ".") || ($file == "..") || ($file == ".RData") || ($file == "STATUS") || ($file == "plot.out")) {
		continue;
	}
	$logs .= "<option>$file</option>\n";
}

# check the pft folder
$pfts = array();
foreach(scandir("$folder/pft") as $pft) {
	if (!is_dir("$folder/pft/$pft") || ($pft == ".") || ($pft == "..")) {
		continue;
	}
	$pfts[$pft] = array();
	foreach(scandir("$folder/pft/${pft}") as $file) {
		if (is_dir("$folder/pft/$pft/$file")) {
			continue;
		}
		$pfts[$pft][] = $file;
		// if (preg_match("/.pdf$/", $file)) {
		// 	$pfts[$pft][] = $file;
		// }
		// if (preg_match("/.log$/", $file)) {
		// 	$pfts[$pft][] = $file;
		// }
	}
}

# check the run output folder
$runfile = array();
$runplot = array();
foreach(scandir("$folder/out") as $runid) {
	if (!is_dir("$folder/out/$runid") || ($runid == ".") || ($runid == "..")) {
		continue;
	}
	$runfile[$runid] = array();
	$runplots[$runid] = array();
	foreach(scandir("$folder/out/$runid") as $file) {
		if (is_dir("$folder/out/$runid/$file")) {
			continue;
		}
		if (preg_match('/^\d\d\d\d.nc.var$/', $file)) {
			continue;
		}

		$runfile[$runid][] = $file;

		if (preg_match('/^\d\d\d\d.nc$/', $file)) {
			$year = substr($file, 0, 4);
			$vars = explode("\n", file_get_contents("${folder}/out/${runid}/${file}.var"));
			$runplot[$runid][$year] = array_filter($vars);
		}
	}
}

?>
<!DOCTYPE html>
<html>
<head>
<title>EBI Results</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
	var pfts = new Array();
<?php
	foreach($pfts as $key => $val) {
		if (count($val) == 0) {
			continue;
		}
		$first = true;
		print "	pfts['$key'] = Array(";
		foreach($val as $x) {
			if ($first) {
				print "'$x'";
			} else {
				print ", '$x'";
			}
			$first = false;
		}
		print ");\n";
	}
?>

	var runfile = new Array();
<?php
	foreach($runfile as $key => $val) {
		if (count($val) == 0) {
			continue;
		}
		print "	runfile['$key'] = Array(";
		$first = true;
		foreach($val as $x) {
			if ($first) {
				print "'$x'";
			} else {
				print ", '$x'";
			}
			$first = false;
		}
		print ");\n";
	}
?>

	var runplot = new Array();
<?php
	foreach($runplot as $key => $val) {
		if (count($val) == 0) {
			continue;
		}
		print "	runplot['$key'] = Array();\n";
		foreach($val as $x => $y) {
			if (count($y) == 0) {
				continue;
			}
			print "	runplot['$key']['$x'] = {};\n";
			foreach($y as $s) {
				$kv = explode(" ", $s, 2);
				if ($kv[1] == '') $kv[1] = $kv[0];
				print "	runplot['$key']['$x']['{$kv[0]}'] = '${kv[1]}';\n";
			}
		}
	}
?>
	function prevStep() {
		$("#formprev").submit();
	}

	function nextStep() {
		$("#formnext").submit();
	}
	
	function showPftOutput(pft, output) {
		show("pft/" + pft + "/" + output);
	}

	function showRunOutput(run, file) {
		show("out/" + run + "/" + file);
	}

	function showRunYearVarPlot(run, year, variable) {
		var url="dataset.php?workflowid=<?=$workflowid?>&type=plot&run=" + run + "&year=" + year + "&var=" + variable + "&width=" + ($("#output").width()-10) + "&height=" + ($("#output").height() - 10);
		$("#output").html("<img src=\"" + url + "\">");		
	}

	function showPEcAnOutput(file) {
		show(file);
	}

	function show(name) {
		var url="dataset.php?workflowid=<?=$workflowid?>&type=file&name=" + name;
		if (endsWith(url, ".xml")) {
			jQuery.get(url, {}, function(data) {
				setOuput((new XMLSerializer()).serializeToString(data));
			});
		} else if (endsWith(url, ".txt") || endsWith(url, ".R") || endsWith(url, ".pavi") || 
			       endsWith(url, ".log") || endsWith(url, ".Rout") || endsWith(url, ".out") ||
			       endsWith(url, ".bug")) {
			jQuery.get(url, {}, setOuput);
		} else if (endsWith(url, ".pdf")) {
			$("#output").html("<object data=\"" + url + "\" type=\"application/pdf\" width=\"100%\" height=\"99%\" >" +
							  "alt : <a href=\"" + url + "\">Click here to download the PDF document</a>" +
							  "</object>");
		} else if (url.indexOf("c.ENS") != -1) {
			jQuery.get(url, {}, setOuput);
		} else if (url.indexOf("ED2IN.template") != -1) {
			jQuery.get(url, {}, setOuput);
		} else {
			window.location = url;
		}
	}

	function setOuput(data) {
		data = data.replace(/&/g, "&amp;").replace(/</g,"&lt;").replace(/>/g, "&gt;").replace(/\"/g, "&quot;");
		$("#output").html("<pre>" + data + "</pre>");
	}

	function updatePFToutput(pft) {
		$('#pftOutput').empty();
		$.each(pfts[pft], function(key, value) {   
		     $('#pftOutput')
		         .append($("<option></option>")
//		         .attr("value",key)
		         .text(value)); 
		});
	}

	function updateOutputRun(run) {
		$('#outfile').empty();
		$.each(runfile[run], function(key, value) {   
		     $('#outfile')
		         .append($("<option></option>")
//		         .attr("value",key)
		         .text(value)); 
		});
		$('#outyear').empty();
		$.each(Object.keys(runplot[run]), function(key, value) {
		     $('#outyear')
		         .append($("<option></option>")
//		         .attr("value",key)
		         .text(value)); 
		});
		year = $('#outyear')[0].value;
		$('#outvar').empty();
		$.each(runplot[run][year], function(key, value) {
		     $('#outvar')
		         .append($("<option></option>")
		         .attr("value",key)
		         .text(value)); 
		});
	}

	function updateOutputYear(run, year) {
		$('#outvar').empty();
		$.each(runplot[run][year], function(key, value) {
		     $('#outvar')
		         .append($("<option></option>")
		         .attr("value",key)
		         .text(value)); 
		});
	}

	function endsWith(haystack, needle) {
		return (haystack.substr(haystack.length - needle.length) === needle);
	}

	function startsWith(haystack, needle) {
		return (haystack.substr(0, needle.length) === needle);
	}
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form action="#" id="form">
			<h1>Results</h1>
			<p>Output from PEcAn execution.</p>
			
			<h2>Outputs</h2>

			<label>Run ID</label>
			<select id="outrun" onChange="updateOutputRun($('#outrun')[0].value);">
<?php
	foreach($runfile as $key => $val) {
		if (count($val) == 0) {
			continue;
		}
		print "				<option>$key</option>\n";
	}
?>
			</select>
			<div class="spacer"></div>
			<label>File</label>
			<select id="outfile">
			</select>
			<div class="spacer"></div>

			<input id="home" type="button" value="Show Run Output" onclick="showRunOutput($('#outrun')[0].value, $('#outfile')[0].value);" />

			<label>Year</label>
			<select id="outyear" onChange="updateOuputYear($('#outrun')[0].value, $('#outyear')[0].value);">
			</select>
			<div class="spacer"></div>
			
			<label>Variable</label>
			<select id="outvar">
			</select>
			<div class="spacer"></div>
			
			<input id="home" type="button" value="Plot run/year/variable" onclick="showRunYearVarPlot($('#outrun')[0].value, $('#outyear')[0].value, $('#outvar')[0].value);" />
			<div class="spacer"></div>

			<p></p>

			<h2>PFTs</h2>
			<label>PFT</label>
			<select id="pft" onChange="updatePFToutput($('#pft')[0].value);">

<?php
	foreach($pfts as $key => $val) {
		if (count($val) == 0) {
			continue;
		}
		print "				<option>$key</option>\n";
	}
?>
			</select>
			<div class="spacer"></div>
			
			<label>Output</label>
			<select id="pftOutput">
			</select>
			<div class="spacer"></div>

			<input id="home" type="button" value="Show PFT Output" onclick="showPftOutput($('#pft')[0].value, $('#pftOutput')[0].value);" />
			<div class="spacer"></div>

			<p></p>

			<h2>PEcAn Files</h2>
			<select id="log">
				<?=$logs?>
			</select>
			<div class="spacer"></div>
			<input id="home" type="button" value="Show File" onclick="showPEcAnOutput($('#log')[0].value);" />
			
			<div class="spacer"></div>
		</form>
		
		<form id="formprev" method="POST" action="history.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
		</form>
		
		<form id="formnext" method="POST" action="02-modelsite.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
		</form>

		<p></p>
		<span id="error" class="small">&nbsp;</span>
		<input id="prev" type="button" value="History" onclick="prevStep();" />
		<input id="next" type="button" value="Start Over" onclick="nextStep();"/>		
		<div class="spacer"></div>
<?php
	if (check_login()) {
		echo "<p></p>";
		echo "Logged in as " . get_user_name();
		echo "<a href=\"index.php?logout\" id=\"logout\">logout</a>";
	}
?>		
	</div>
	<div id="output">Please select an option on the left.</div>
	<div id="footer">
		The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
		(ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
		Biosciences Institute</a>.
	</div>
</div>
</body>
	<script type="text/javascript">
		updatePFToutput($('#pft')[0].value);
		updateOutputRun($('#outrun')[0].value);
		updateOutputYear($('#outrun')[0].value, $('#outyear')[0].value);
	</script>
</html>

<?php 
$pdo = null;
?>
