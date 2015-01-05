<?php	
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// what site is earth
$earth=1118;

// Check login
require("common.php");
open_database();
if ($authentication) {
	if (!check_login()) {
		close_database();
		header('HTTP/1.1 403 Unauthorized');
		exit;
	}
}

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$dom->formatOutput = true;
$node = $dom->createElement("markers");
$parnode = $dom->appendChild($node); 

$query = "SELECT DISTINCT sites.* FROM sites";

// 1. Get a list of all sites we have
$sites = array();
$result = $pdo->query("SELECT id, sitename, city, country, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites;");
if (!$result) {
  die('Invalid query: ' . error_database());
} 
while ($row = @$result->fetch(PDO::FETCH_ASSOC)) {
  $row['format_id'] = array();
  $sites[$row['id']] = $row;
} 

// if model and host given return list of possible sites
if (isset($_REQUEST['model']) && ($_REQUEST['model'] != "") && isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {
  // 2. Get list of all formats for each site
  $stmt = $pdo->prepare("SELECT DISTINCT sites.id, format_id FROM sites, inputs, dbfiles, machines" .
                        " WHERE sites.id=inputs.site_id AND inputs.id=dbfiles.container_id" .
                        " AND dbfiles.container_type='Input' and dbfiles.machine_id=machines.id" .
                        " AND machines.hostname=? GROUP BY sites.id, format_id;");
  if (!$stmt->execute(array($_REQUEST['host']))) {
    die('Invalid query: ' . error_database());
  }
  while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
    $sites[$row['id']]['format_id'][] = $row['format_id'];
  } 
  $stmt->closeCursor();

  // 3. Find all formats that are in world
  $stmt = $pdo->prepare("SELECT DISTINCT format_id FROM inputs, dbfiles, machines" .
                        " WHERE inputs.site_id=${earth} AND inputs.id=dbfiles.container_id" .
                        " AND dbfiles.container_type='Input' and dbfiles.machine_id=machines.id" .
                        " AND machines.hostname=? GROUP BY format_id;");
  if (!$stmt->execute(array($_REQUEST['host']))) {
    die('Invalid query: ' . error_database());
  }
  while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
    foreach($sites as &$site) {
      $site['format_id'][] = $row['format_id'];
    }
  } 
  $stmt->closeCursor();

  // 4. Find all MET conversions possible
  // 4a. Check for Download Ameriflux -> CF
  foreach($sites as &$site) {
    if (preg_match("/ \(US-.*\)$/", $site["sitename"])) {
      if (!in_array(33, $site['format_id'])) {
        $site['format_id'][] = 33;
      }
    }
  }

  // 4b. Check for CF - > SIPNET/ED
  $stmt = $pdo->prepare("SELECT modeltypes.name FROM modeltypes, models" .
                        " WHERE modeltypes.id=models.modeltype_id" .
                        " AND models.id=?;");
  if (!$stmt->execute(array($_REQUEST['model']))) {
    die('Invalid query: ' . error_database());
  }
  $modeltypes=$stmt->fetchAll(PDO::FETCH_COLUMN, 0);
  $stmt->closeCursor();
  if (in_array("SIPNET", $modeltypes)) {
    foreach($sites as &$site) {
      if (in_array(33, $site['format_id'])) {
        $site['format_id'][] = 24;
      }
    }
  }
  if (in_array("ED2", $modeltypes)) {
    foreach($sites as &$site) {
      if (in_array(33, $site['format_id'])) {
        $site['format_id'][] = 12;
      }
    }
  }
  if (in_array("DALEC", $modeltypes)) {
    foreach($sites as &$site) {
      if (in_array(33, $site['format_id'])) {
        $site['format_id'][] = 1000000002;
      }
    }
  }


  // 5. Find any other conversions if they exist


  // 6. Get list of all formats needed for model
  $stmt = $pdo->prepare("SELECT format_id FROM modeltypes_formats, models" .
                        " WHERE modeltypes_formats.modeltype_id=models.modeltype_id" .
                        " AND models.id=?;");
  if (!$stmt->execute(array($_REQUEST['model']))) {
    die('Invalid query: ' . error_database());
  }
  $formats=$stmt->fetchAll(PDO::FETCH_COLUMN, 0);
  $stmt->closeCursor();

  // Filter all sites that are not formats
  $filtered = array();
  foreach($sites as $site) {
    if (count(array_diff($formats, $site['format_id'])) == 0) {
      $filtered[] = $site;
    }
  }
  $sites = $filtered;
}
close_database();

// return a list of all all sites that have all formats
foreach($sites as $site) {
  $node = $dom->createElement("marker");
  $newnode = $parnode->appendChild($node);   
  $newnode->setAttribute("siteid",$site['id']);
  $newnode->setAttribute("city", $site['city']);
  $newnode->setAttribute("country", $site['country']); 
  $newnode->setAttribute("lat", $site['lat']); 
  $newnode->setAttribute("lon", $site['lon']);
  if ($site['sitename'] != "") {
    $newnode->setAttribute("sitename", $site['sitename']);
  } else {  
    $newnode->setAttribute("sitename", $site['id'] . " - " . $site['city']);
  }
}

echo $dom->saveXML();
?>
