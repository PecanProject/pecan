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

// allow for switching x-axis
$xaxis=isset($_REQUEST['xaxis']);
$xaxis=TRUE;

// get run information
$stmt = $pdo->prepare("SELECT * FROM workflows WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();
$start = substr($workflow['start_date'], 0, 4);
$end = substr($workflow['end_date'], 0, 4);
$folder = $workflow['folder'];

# check to make sure all is ok
$error=false;
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
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
} else {
  $error=true;
}

# check the PEcAn folder
$pecanfiles = array();
foreach(scandir("$folder") as $file) {
  if (is_dir("$folder/$file") || ($file == ".") || ($file == "..") || ($file == ".RData") || ($file == "plot.out")) {
    continue;
  }
  $pecanfiles[] = $file;
}
foreach(recursive_scandir("$folder/ensemble", "ensemble") as $file) {
  $pecanfiles[] = $file;
}
foreach(recursive_scandir("$folder/sensitivity", "sensitivity") as $file) {
  $pecanfiles[] = $file;
}

# check the pft folder
$pfts = array();
if (is_dir("$folder/pft")) {
  foreach(scandir("$folder/pft") as $pft) {
    if (!is_dir("$folder/pft/$pft") || ($pft == ".") || ($pft == "..")) {
      continue;
    }
    $pfts[$pft] = array();
    foreach(recursive_scandir("$folder/pft/${pft}", "") as $file) {
      if (is_dir("$folder/pft/$pft/$file")) {
        continue;
      }
      $pfts[$pft][] = $file;
    }
  }
}

# check the run folders
$inpfile = array();
$outfile = array();
$outplot = array();
if (is_dir("$folder/run")) {
  foreach(scandir("$folder/run") as $runid) {
    if (!is_dir("$folder/run/$runid") || ($runid == ".") || ($runid == "..")) {
      continue;
    }

    # input files
    $inpfile[$runid] = array();
    foreach(scandir("$folder/run/$runid") as $file) {
      if (is_dir("$folder/run/$runid/$file")) {
        continue;
      }
      $inpfile[$runid][] = $file;
    }

    # output files
    $outfile[$runid] = array();
    $outplot[$runid] = array();
    foreach(scandir("$folder/out/$runid") as $file) {
      if (is_dir("$folder/out/$runid/$file")) {
        continue;
      }
      if (preg_match('/^\d\d\d\d.nc.var$/', $file)) {
        continue;
      }
      $outfile[$runid][] = $file;
      if (preg_match('/^\d\d\d\d.nc$/', $file)) {
        $year = substr($file, 0, 4);
        $vars = explode("\n", file_get_contents("${folder}/out/${runid}/${file}.var"));
        $outplot[$runid][$year] = array_filter($vars);
        sort($outplot[$runid][$year]);
      }
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
    print "  pfts['$key'] = Array(";
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

  var inpfile = new Array();
<?php
  foreach($inpfile as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print " inpfile['$key'] = Array(";
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

  var outfile = new Array();
<?php
  foreach($outfile as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print " outfile['$key'] = Array(";
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

  var outplot = new Array();
<?php
  foreach($outplot as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print "  outplot['$key'] = Array();\n";
    foreach($val as $x => $y) {
      if (count($y) == 0) {
        continue;
      }
      print "  outplot['$key']['$x'] = {};\n";
      foreach($y as $s) {
        $kv = explode(" ", $s, 2);
        if ($kv[1] == '') $kv[1] = $kv[0];
        print "  outplot['$key']['$x']['{$kv[0]}'] = '${kv[1]}';\n";
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
  
  function showGraph() {
    var run = $('#runid').val();
    var year = $('#graphyear').val();
    var xvar = $('#graphxvar').val();
    var yvar = $('#graphyvar').val();
    var url="dataset.php?workflowid=<?php echo $workflowid; ?>&type=plot&run=" + run + "&year=" + year + "&xvar=" + xvar + "&yvar=" + yvar + "&width=" + ($("#output").width()-10) + "&height=" + ($("#output").height() - 10);
    $("#output").html("<img src=\"" + url + "\">");    
  }

  function showInputFile() {
    var run = $('#runid').val();
    var file = $('#inpfile').val();
    show("run/" + run + "/" + file);
  }

  function showOutputFile() {
    var run = $('#runid').val();
    var file = $('#outfile').val();
    show("out/" + run + "/" + file);
  }

  function showPFT() {
    var pft = $('#pft').val();
    var file = $('#pftfile').val();
    show("pft/" + pft + "/" + file);
  }

  function showPEcAnFile() {
    show($('#pecanfile').val());
  }

  function show(name) {
    var url="dataset.php?workflowid=<?php echo $workflowid; ?>&type=file&name=" + name;
    if (endsWith(url, ".xml")) {
      jQuery.get(url, {}, function(data) {
        setOuput((new XMLSerializer()).serializeToString(data));
      });
    } else if (endsWith(url, ".pdf")) {
      $("#output").html("<object data=\"" + url + "\" type=\"application/pdf\" width=\"100%\" height=\"99%\" >" +
                "alt : <a href=\"" + url + "\">Click here to download the PDF document</a>" +
                "</object>");
    } else if (endsWith(url, ".nc") || endsWith(url, ".h5") || endsWith(url, ".rdata")) {
      window.location = url;
    } else {
      jQuery.get(url, {}, setOuput);
    }
  }

  function setOuput(data) {
    data = data.replace(/&/g, "&amp;").replace(/</g,"&lt;").replace(/>/g, "&gt;").replace(/\"/g, "&quot;");
    $("#output").html("<pre>" + data + "</pre>");
  }

  function updatePFT() {
    var pft = $('#pft').val();
    $('#pftfile').empty();
    $.each(pfts[pft], function(key, value) {   
         $('#pftfile')
             .append($("<option></option>")
             .text(value)); 
    });
  }

  function updateRun() {
    var run = $('#runid').val();

    $('#graphyear').empty();
    if (outplot[run]) {
      $.each(Object.keys(outplot[run]), function(key, value) {
           $('#graphyear')
               .append($("<option></option>")
               .text(value)); 
      });
      updateGraphYear();
    }

    $('#inpfile').empty();
    $.each(inpfile[run], function(key, value) {   
         $('#inpfile')
             .append($("<option></option>")
             .text(value)); 
    });

    $('#outfile').empty();
    $.each(outfile[run], function(key, value) {   
         $('#outfile')
             .append($("<option></option>")
             .text(value)); 
    });
  }

  function updateGraphYear() {
    var run = $('#runid').val();
    var year = $('#graphyear').val();

<?php if ($xaxis) { ?>
    $('#graphxvar').empty();
<?php } ?>
    $('#graphyvar').empty();
    $.each(outplot[run][year], function(key, value) {
         $('#graphxvar')
             .append($("<option></option>")
                .attr("value", key)
                .text(value)); 
         $('#graphyvar')
             .append($("<option></option>")
                .attr("value", key)
                .text(value)); 
    });
<?php if ($xaxis) { ?>
    $('#graphxvar')
             .append($("<option></option>")
                .attr("value", "time")
                .text("time")
                .prop("selected", true));
<?php } ?>
    $('#graphyvar')
             .append($("<option></option>")
                .attr("value", "time")
                .text("time"));
  }

  function endsWith(haystack, needle) {
    haystack = haystack.toLowerCase();
    needle = needle.toLowerCase();
    return (haystack.substr(haystack.length - needle.length) === needle);
  }

  function startsWith(haystack, needle) {
    haystack = haystack.toLowerCase();
    needle = needle.toLowerCase();
    return (haystack.substr(0, needle.length) === needle);
  }
</script>
</head>
<body>
<div id="wrap">
  <div id="stylized">
    <form action="#" id="form">
      <h1>Results</h1>
<?php
 if (count($inpfile) < 2) {
  reset($inpfile);
  $key = key($inpfile);
?>
      <span class="small">Only 1 run (<?php echo $key; ?>)</span>
      <input id="runid" type="hidden" value="<?php echo $key; ?>" />
<?php } else { ?>
      <span class="small">Select a specific run.</span>
      <label>Run ID</label>
      <select id="runid" onChange="updateRun();">
<?php
  foreach($inpfile as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print "        <option>$key</option>\n";
  }
?>
      </select>
<?php } ?>

      <p></p>
      <h1>Graphs</h1>

      <label>Year</label>
      <select id="graphyear" onChange="updateGraphYear();">
      </select>
      <div class="spacer"></div>
      
<?php if ($xaxis) { ?>
      <label>X-axis</label>
      <select id="graphxvar">
      </select>
      <div class="spacer"></div>
<?php } else { ?>
      <input type="hidden" id="graphxvar" value="time"/>
<?php } ?>

<?php if ($xaxis) { ?>
      <label>Y-axis</label>
<?php } else { ?>
      <label>Variable</label>
<?php } ?>
      <select id="graphyvar">
      </select>
      <div class="spacer"></div>
      
      <input id="home" type="button" value="Plot run/year/variable" onclick="showGraph();" />

      <p></p>
      <h1>Inputs</h1>

      <label>File</label>
      <select id="inpfile">
      </select>
      <div class="spacer"></div>

      <input id="home" type="button" value="Show Input File" onclick="showInputFile();" />

      <p></p>
      <h1>Outputs</h1>

      <label>File</label>
      <select id="outfile">
      </select>
      <div class="spacer"></div>

      <input id="home" type="button" value="Show Output File" onclick="showOutputFile();" />

      <p></p>
      <h1>PFTs</h1>
      <label>PFT</label>
      <select id="pft" onChange="updatePFT();">

<?php
  foreach($pfts as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print "        <option>$key</option>\n";
  }
?>
      </select>
      <div class="spacer"></div>
      
      <label>Output</label>
      <select id="pftfile">
      </select>
      <div class="spacer"></div>

      <input id="home" type="button" value="Show PFT Output" onclick="showPFT();" />

      <p></p>
      <h1>PEcAn Files</h1>
      <select id="pecanfile">
<?php
  foreach($pecanfiles as $file) {
    print "        <option>$file</option>\n";
  }
?>
      </select>
      <div class="spacer"></div>
      <input id="home" type="button" value="Show File" onclick="showPEcAnFile();" />
      
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
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
  <script type="text/javascript">
    updateRun();
    updatePFT();
  </script>
</html>

<?php 
$pdo = null;

function recursive_scandir($dir, $base) {
  $files = array();
  foreach (array_diff(scandir($dir), array('.','..')) as $file) {
    if (is_dir("$dir/$file")) {
      if ($base == "") {
        $files = array_merge($files, recursive_scandir("$dir/$file", "$file"));
      } else {
        $files = array_merge($files, recursive_scandir("$dir/$file", "$base/$file"));
      }
    } else {
      if ($base == "") {
        $files[] = $file;
      } else {
        $files[] = "$base/$file"; 
      }
    }
  }
  return $files;
}
?>
