<?php  
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
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
$userok=isset($_REQUEST['userok']);
$offline=isset($_REQUEST['offline']);
$pecan_edit=isset($_REQUEST['pecan_edit']);
$model_edit=isset($_REQUEST['model_edit']);
$browndog=isset($_REQUEST['browndog']);

# parameters
if (!isset($_REQUEST['siteid'])) {
  die("Need a siteid.");
}
$siteid=$_REQUEST['siteid'];
if (!isset($_REQUEST['modelid'])) {
  die("Need a modelid.");
}
$modelid=$_REQUEST['modelid'];
if (!isset($_REQUEST['hostname'])) {
  die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
if (!isset($_REQUEST['pft'])) {
	die("Need a pft.");
}
$pft=$_REQUEST['pft'];

# dates
if (!isset($_REQUEST['start'])) {
  die("Need a start date.");
}
$startdate=$_REQUEST['start'];
$metstart=$startdate;
if (!isset($_REQUEST['end'])) {
  die("Need a end date.");
}
$enddate=$_REQUEST['end'];
$metend=$enddate;

# non required parameters
$email = "";
if (isset($_REQUEST['email'])) {
	$email = $_REQUEST['email'];
}

# check met info
if (isset($_REQUEST['input_met']) && is_numeric($_REQUEST['input_met'])) {
  $stmt = $pdo->prepare("SELECT start_date, end_date FROM inputs WHERE inputs.id=?");
  if (!$stmt->execute(array($_REQUEST['input_met']))) {
    die('Invalid query: ' . (error_database()));
  }
  $row = $stmt->fetch(PDO::FETCH_ASSOC);
  $metstart=$row['start_date'];
  $metend=$row['end_date'];
  $stmt->closeCursor();
}

// check input dates to make sure they agree with the dates from the weather data
if (!$userok && ($startdate < $metstart || $enddate > $metend)) {
	$params = "userok=on";
	foreach($_REQUEST as $k => $v) {
		if (is_array($v)) {
			foreach($v as $x) {
				$params .= "&${k}[]=$x";
			}
		} else {
			$params .= "&${k}=$v";
		}
	}
	$params .= "&msg=WARNING : Selected dates are not within the bounds of the weather data file you selected.";
	header("Location: checkfailed.php?${params}");
	exit();
}

// get model info
$stmt = $pdo->prepare("SELECT name, revision FROM models, modeltypes WHERE models.id=? AND modeltypes.id=models.modeltype_id");
if (!$stmt->execute(array($modelid))) {
  die('Invalid query: ' . (error_database()));
}
$row = $stmt->fetch(PDO::FETCH_ASSOC);
$revision=$row['revision'];
$modeltype=$row['name'];
$stmt->closeCursor();

// create the workflow execution
$params=str_replace(' ', '', str_replace("\n", "", var_export($_REQUEST, true)));

$q=$pdo->prepare("INSERT INTO workflows (site_id, model_id, hostname, start_date, end_date, params, advanced_edit, started_at, created_at) values (:siteid, :modelid, :hostname, :startdate, :enddate, :params, :advanced_edit, NOW(), NOW())");
$q->bindParam(':siteid', $siteid, PDO::PARAM_INT);
$q->bindParam(':modelid', $modelid, PDO::PARAM_INT);
$q->bindParam(':hostname', $hostname, PDO::PARAM_STR);
$q->bindParam(':startdate', $startdate, PDO::PARAM_STR);
$q->bindParam(':enddate', $enddate, PDO::PARAM_STR);
$q->bindParam(':params', $params, PDO::PARAM_STR);
$advanced_edit = ($pecan_edit || $model_edit) ? "yes" : "no";
$q->bindParam(':advanced_edit', $advanced_edit, PDO::PARAM_INT);
if ($q->execute() === FALSE) {
  die('Can\'t insert workflow : ' . (error_database()));
}
if ($db_bety_type == 'pgsql') {
  $workflowid=$pdo->lastInsertId('workflows_id_seq');
} else {
  $workflowid=$pdo->lastInsertId();
}

# folders
$folder = $output_folder . DIRECTORY_SEPARATOR . 'PEcAn_' . $workflowid;
if ($pdo->query("UPDATE workflows SET folder='${folder}' WHERE id=${workflowid}") === FALSE) {
  die('Can\'t update workflow : ' . (error_database()));
}

# if on localhost replace with localhost
if ($hostname == $fqdn) {
	$hostname="localhost";
}

# create pecan.xml
if (!mkdir($folder)) {
	die('Can\'t create output folder');
}
$fh = fopen($folder . DIRECTORY_SEPARATOR . "pecan.xml", 'w');
fwrite($fh, "<?xml version=\"1.0\"?>" . PHP_EOL);
fwrite($fh, "<pecan>" . PHP_EOL);

fwrite($fh, "  <outdir>${folder}</outdir>" . PHP_EOL);

fwrite($fh, "  <database>" . PHP_EOL);

fwrite($fh, "    <bety>" . PHP_EOL);
fwrite($fh, "      <user>${db_bety_username}</user>" . PHP_EOL);
fwrite($fh, "      <password>${db_bety_password}</password>" . PHP_EOL);
fwrite($fh, "      <host>${db_bety_hostname}</host>" . PHP_EOL);
fwrite($fh, "      <dbname>${db_bety_database}</dbname>" . PHP_EOL);
if ($db_bety_type == "mysql") {
	fwrite($fh, "      <driver>MySQL</driver>" . PHP_EOL);	
} else if ($db_bety_type = "pgsql") {
	fwrite($fh, "      <driver>PostgreSQL</driver>" . PHP_EOL);	
}
fwrite($fh, "      <write>true</write>" . PHP_EOL);
fwrite($fh, "    </bety>" . PHP_EOL);

if (isset($db_fia_database) && ($db_fia_database != "")) {
	fwrite($fh, "    <fia>" . PHP_EOL);
	fwrite($fh, "      <user>${db_fia_username}</user>" . PHP_EOL);
	fwrite($fh, "      <password>${db_fia_password}</password>" . PHP_EOL);
	fwrite($fh, "      <host>${db_fia_hostname}</host>" . PHP_EOL);
	fwrite($fh, "      <dbname>${db_fia_database}</dbname>" . PHP_EOL);
	if ($db_fia_type == "mysql") {
		fwrite($fh, "      <driver>MySQL</driver>" . PHP_EOL);	
	} else if ($db_fia_type = "pgsql") {
		fwrite($fh, "      <driver>PostgreSQL</driver>" . PHP_EOL);	
	}
	fwrite($fh, "    </fia>" . PHP_EOL);
}

fwrite($fh, "  </database>" . PHP_EOL);

if ($browndog) {
  fwrite($fh, "  <browndog>" . PHP_EOL);  
  fwrite($fh, "    <url>${browndog_url}</url>" . PHP_EOL);  
  fwrite($fh, "    <username>${browndog_username}</username>" . PHP_EOL);  
  fwrite($fh, "    <password>${browndog_password}</password>" . PHP_EOL);  
  fwrite($fh, "  </browndog>" . PHP_EOL);  
}

$pft_id=1;
fwrite($fh, "  <pfts>" . PHP_EOL);
foreach($pft as $p) {
	fwrite($fh, "    <pft>" . PHP_EOL);
	fwrite($fh, "      <name>${p}</name> " . PHP_EOL);
	fwrite($fh, "      <constants>" . PHP_EOL);
	fwrite($fh, "        <num>${pft_id}</num>" . PHP_EOL);
	fwrite($fh, "      </constants>" . PHP_EOL);
	fwrite($fh, "    </pft>" . PHP_EOL);
	$pft_id++;
}
fwrite($fh, "  </pfts>" . PHP_EOL);

fwrite($fh, "  <meta.analysis>" . PHP_EOL);
fwrite($fh, "    <iter>3000</iter>" . PHP_EOL);
fwrite($fh, "    <random.effects>FALSE</random.effects>" . PHP_EOL);
fwrite($fh, "  </meta.analysis>" . PHP_EOL);

fwrite($fh, "  <ensemble>" . PHP_EOL);
fwrite($fh, "    <size>1</size>" . PHP_EOL);
fwrite($fh, "    <variable>NPP</variable>" . PHP_EOL);
fwrite($fh, "  </ensemble>" . PHP_EOL);

fwrite($fh, "  <model>" . PHP_EOL);
fwrite($fh, "    <id>${modelid}</id>" . PHP_EOL);
if ($modeltype == "ED2") {
	fwrite($fh, "    <config.header>" . PHP_EOL);
	fwrite($fh, "      <radiation>" . PHP_EOL);
	fwrite($fh, "        <lai_min>0.01</lai_min>" . PHP_EOL);
	fwrite($fh, "      </radiation>" . PHP_EOL);
	fwrite($fh, "      <ed_misc>" . PHP_EOL);
	fwrite($fh, "        <output_month>12</output_month>      " . PHP_EOL);
	fwrite($fh, "      </ed_misc> " . PHP_EOL);
	fwrite($fh, "    </config.header>" . PHP_EOL);
	fwrite($fh, "    <edin>ED2IN.r${revision}</edin>" . PHP_EOL);
	fwrite($fh, "    <phenol.scheme>0</phenol.scheme>" . PHP_EOL);
}
fwrite($fh, "  </model>" . PHP_EOL);
fwrite($fh, "  <workflow>" . PHP_EOL);
fwrite($fh, "    <id>$workflowid</id>" . PHP_EOL);
fwrite($fh, "  </workflow>" . PHP_EOL);
fwrite($fh, "  <run>" . PHP_EOL);
fwrite($fh, "    <site>" . PHP_EOL);
fwrite($fh, "      <id>${siteid}</id>" . PHP_EOL);
fwrite($fh, "      <met.start>${metstart}</met.start>" . PHP_EOL);
fwrite($fh, "      <met.end>${metend}</met.end>" . PHP_EOL);
fwrite($fh, "    </site>" . PHP_EOL);
fwrite($fh, "    <inputs>" . PHP_EOL);
foreach($_REQUEST as $key => $val) {
  if (substr($key, 0, 6) != "input_") continue;
  $tag=substr($key, 6);
  fwrite($fh, "      <${tag}>" . PHP_EOL);
  if (is_numeric($val)) {
    fwrite($fh, "        <id>${val}</id>" . PHP_EOL);
  } else {
    $parts=explode(".", $val, 2);
    fwrite($fh, "        <source>${parts[0]}</source>" . PHP_EOL);
    fwrite($fh, "        <output>${parts[1]}</output>" . PHP_EOL);
  }
  fwrite($fh, "      </${tag}>" . PHP_EOL);
}
fwrite($fh, "    </inputs>" . PHP_EOL);
fwrite($fh, "    <start.date>${startdate}</start.date>" . PHP_EOL);
fwrite($fh, "    <end.date>${enddate}</end.date>" . PHP_EOL);
fwrite($fh, "    <dbfiles>${output_folder}</dbfiles>" . PHP_EOL);
fwrite($fh, "    <host>" . PHP_EOL);
fwrite($fh, "      <name>${hostname}</name>" . PHP_EOL);
fwrite($fh, "    </host>" . PHP_EOL);
fwrite($fh, "  </run>" . PHP_EOL);
if ($email != "") {
	$url = ($_SERVER['HTTPS'] ? "https://" : "http://");
	$url .= $_SERVER['HTTP_HOST'] . ':' . $_SERVER['SERVER_PORT'];
	$url .= str_replace("04-runpecan.php", "08-finished.php", $_SERVER["SCRIPT_NAME"]);
	if ($offline) {
		$url .= "?workflowid=${workflowid}&offline=offline";
	} else {
		$url .= "?workflowid=${workflowid}";
	}
	fwrite($fh, "  <email>" . PHP_EOL);
	fwrite($fh, "    <to>${email}</to>" . PHP_EOL);
	fwrite($fh, "    <url>${url}</url>" . PHP_EOL);
	fwrite($fh, "  </email>" . PHP_EOL);
}
fwrite($fh, "</pecan>" . PHP_EOL);
fclose($fh); 

# copy workflow
copy("workflow.R", "${folder}/workflow.R");

if ($pecan_edit) {
  $path = "06-edit.php?workflowid=$workflowid&pecan_edit=pecan_edit";
  if ($model_edit) {
    $path .= "&model_edit=model_edit";
  }
  if ($offline) {
    $path .= "&offline=offline";
  }
  header("Location: ${path}");
} else {
  # start the actual workflow
  chdir($folder);

  if ($model_edit) {
    pclose(popen('R_LIBS_USER="' . $pecan_install . '" ' . $Rbinary . ' CMD BATCH --advanced  workflow.R &', 'r')); 
  } else {
    pclose(popen('R_LIBS_USER="' . $pecan_install . '" ' . $Rbinary . ' CMD BATCH workflow.R &', 'r')); 
  }

  #done
  $path = "05-running.php?workflowid=$workflowid";
  if ($pecan_edit) {
    $path .= "&pecan_edit=pecan_edit";
  }
  if ($model_edit) {
    $path .= "&model_edit=model_edit";
  }
  if ($offline) {
    $path .= "&offline=offline";
  }
  header("Location: ${path}");
}

close_database();
?>

