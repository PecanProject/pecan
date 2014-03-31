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
$advanced_edit=isset($_REQUEST['advanced_edit']);

# parameters
if (!isset($_REQUEST['siteid'])) {
  die("Need a siteid.");
}
$siteid=$_REQUEST['siteid'];
if (!isset($_REQUEST['modelid'])) {
  die("Need a modelid.");
}
$modelid=$_REQUEST['modelid'];
if (!isset($_REQUEST['modeltype'])) {
  die("Need a modeltype.");
}
$modeltype=$_REQUEST['modeltype'];
if (!isset($_REQUEST['hostname'])) {
  die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
if (!isset($_REQUEST['pft'])) {
	die("Need a pft.");
}
$pft=$_REQUEST['pft'];

# non required parameters
$email = "";
if (isset($_REQUEST['email'])) {
	$email = $_REQUEST['email'];
}

# specific for each model type
if (($modeltype == "ED2") || ($modeltype == "BIOCRO")) {
	if (!isset($_REQUEST['start'])) {
		die("Need a start date.");
	}
	$startdate=$_REQUEST['start'];
	if (!isset($_REQUEST['end'])) {
		die("Need a end date.");
	}
	$enddate=$_REQUEST['end'];
}
if ($modeltype == "BIOCRO") {
    $metstart=$startdate;
    $metend=$enddate;
}

if (($modeltype == "ED2") || ($modeltype == "SIPNET")) {
	if (!isset($_REQUEST['met'])) {
		die("Need a weather file.");
	}
	$met=$_REQUEST['met'];	
    $query="SELECT file_path, file_name, start_date, end_date FROM inputs, dbfiles, machines WHERE " .
           " inputs.site_id=${siteid} AND inputs.id=${met}" .
           " AND dbfiles.container_id=inputs.id AND dbfiles.container_type='Input'" .
           " AND machines.hostname='${hostname}' AND machines.id=dbfiles.machine_id";
    $result = $pdo->query($query);
    if (!$result) {
      print_r(error_database());
      die('Invalid query: ' . (error_database()));
    }
    $row = $result->fetch(PDO::FETCH_ASSOC);
    $metfile=$row['file_path'] . DIRECTORY_SEPARATOR . $row['file_name'];
    $metstart=$row['start_date'];
    $metend=$row['end_date'];
}
if ($modeltype == "SIPNET") {
    $startdate=$metstart;
    $enddate=$metend;
}

if ($modeltype == "ED2") {
	if (!isset($_REQUEST['psscss'])) {
		die("Need a psscss.");
	}
	$psscss=$_REQUEST['psscss'];
	if($psscss != "FIA") {
 	    $query="SELECT file_path, file_name FROM inputs, dbfiles, machines WHERE" .
                   " inputs.site_id=${siteid} AND inputs.id=${psscss}" .
                   " AND dbfiles.container_id=inputs.id AND dbfiles.container_type='Input'" .
                   " AND machines.hostname='${hostname}' AND machines.id=dbfiles.machine_id";
	    $result = $pdo->query($query);
	    if (!$result) {
	      print_r(error_database());
	      die('Invalid query: ' . (error_database()));
	    }
	    $row = $result->fetch(PDO::FETCH_ASSOC);
	    #$psscss=$row['file_path'] . DIRECTORY_SEPARATOR . $row['file_name'];
	    $psscssfile=substr($row['file_path'], 0, strlen($row['file_path']) - 4);
	}
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
	header("Location: checkfailed.php?{$params}");
	exit();
}

// get site information
$query = "SELECT * FROM sites WHERE sites.id=$siteid";
$result = $pdo->query($query);
if (!$result) {
  print_r(error_database());
  die('Invalid query: ' . (error_database()));
}
$siteinfo = $result->fetch(PDO::FETCH_ASSOC);

// get model information
$query = "SELECT * FROM models WHERE models.id=$modelid";
$result = $pdo->query($query);
if (!$result) {
  print_r(error_database());
  die('Invalid query: ' . (error_database()));
}
$model = $result->fetch(PDO::FETCH_ASSOC);
$pieces = explode(':', $model["model_path"], 2);
$binary = $pieces[1];

// create the workflow execution
$params=str_replace(' ', '', str_replace("\n", "", var_export($_REQUEST, true)));

$q=$pdo->prepare("INSERT INTO workflows (site_id, model_id, hostname, start_date, end_date, params, advanced_edit, started_at, created_at) values (:siteid, :modelid, :hostname, :startdate, :enddate, :params, :advanced_edit, NOW(), NOW())");
$q->bindParam(':siteid', $siteid, PDO::PARAM_INT);
$q->bindParam(':modelid', $modelid, PDO::PARAM_INT);
$q->bindParam(':hostname', $hostname, PDO::PARAM_STR);
$q->bindParam(':startdate', $startdate, PDO::PARAM_STR);
$q->bindParam(':enddate', $enddate, PDO::PARAM_STR);
$q->bindParam(':params', $params, PDO::PARAM_STR);
$q->bindParam(':advanced_edit', $advanced_edit, PDO::PARAM_INT);
if ($q->execute() === FALSE) {
  die('Can\'t insert workflow : ' . (error_database()));
}
if ($db_type == 'pgsql') {
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
if ($hostname == gethostname()) {
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
fwrite($fh, "    <user>$db_username</user>" . PHP_EOL);
fwrite($fh, "    <password>$db_password</password>" . PHP_EOL);
fwrite($fh, "    <host>${db_hostname}</host>" . PHP_EOL);
fwrite($fh, "    <dbname>${db_database}</dbname>" . PHP_EOL);
if ($db_type == "mysql") {
	fwrite($fh, "    <driver>MySQL</driver>" . PHP_EOL);	
} else if ($db_type = "pgsql") {
	fwrite($fh, "    <driver>PostgreSQL</driver>" . PHP_EOL);	
}
fwrite($fh, "  </database>" . PHP_EOL);

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
fwrite($fh, "    <name>$modeltype</name>" . PHP_EOL);
fwrite($fh, "    <binary>${binary}</binary>" . PHP_EOL);
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
	fwrite($fh, "    <edin>ED2IN.r{$model['revision']}</edin>" . PHP_EOL);
	fwrite($fh, "    <veg>${ed_veg}</veg>" . PHP_EOL);
	fwrite($fh, "    <soil>${ed_soil}</soil>" . PHP_EOL);
	if ($psscss == "FIA") {
		fwrite($fh, "    <psscss generate=\"fia\">${folder}/fia.</psscss>" . PHP_EOL);
	} else {
		fwrite($fh, "    <psscss>$psscssfile</psscss>" . PHP_EOL);
	}
	fwrite($fh, "    <inputs>${ed_inputs}</inputs>" . PHP_EOL);
	fwrite($fh, "    <phenol.scheme>0</phenol.scheme>" . PHP_EOL);
}
fwrite($fh, "  </model>" . PHP_EOL);
fwrite($fh, "  <workflow>" . PHP_EOL);
fwrite($fh, "    <id>$workflowid</id>" . PHP_EOL);
fwrite($fh, "  </workflow>" . PHP_EOL);
fwrite($fh, "  <run>" . PHP_EOL);
fwrite($fh, "    <site>" . PHP_EOL);
fwrite($fh, "      <id>${siteid}</id>" . PHP_EOL);
fwrite($fh, "      <name>{$siteinfo['sitename']}</name>" . PHP_EOL);
fwrite($fh, "      <lat>{$siteinfo['lat']}</lat>" . PHP_EOL);
fwrite($fh, "      <lon>{$siteinfo['lon']}</lon>" . PHP_EOL);
if (($modeltype == "ED2") || ($modeltype == "SIPNET")) {
	fwrite($fh, "      <met>$metfile</met>" . PHP_EOL);
}
fwrite($fh, "      <met.start>${metstart}</met.start>" . PHP_EOL);
fwrite($fh, "      <met.end>${metend}</met.end>" . PHP_EOL);
fwrite($fh, "    </site>" . PHP_EOL);
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

# copy ED template
#if ($modeltype == "ED2") {
#	copy("template/{$model['model_name']}_r{$model['revision']}", "${folder}/ED2IN.template");
#}

# copy workflow
copy("workflow.R", "${folder}/workflow.R");

# start the actual workflow
chdir($folder);
if ($advanced_edit) {
	pclose(popen('R_LIBS_USER="' . $pecan_install . '" ' . $Rbinary . ' CMD BATCH --advanced  workflow.R &', 'r'));	
} else {
	pclose(popen('R_LIBS_USER="' . $pecan_install . '" ' . $Rbinary . ' CMD BATCH workflow.R &', 'r'));	
}

#done
if ($offline) {
	header("Location: 05-running.php?workflowid=$workflowid&offline=offline");
} else {
	header("Location: 05-running.php?workflowid=$workflowid");
}

close_database();
?>

