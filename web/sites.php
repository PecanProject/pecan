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
$node = $dom->createElement("markers");
$parnode = $dom->appendChild($node); 

$query = "SELECT DISTINCT sites.* FROM sites";

// if model and host given return list of possible sites
if (isset($_REQUEST['model']) && ($_REQUEST['model'] != "") && isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {

  // 1. Get list of all formats needed for model
  $stmt = $pdo->prepare("SELECT format_id FROM modeltypes_formats, models" .
                        " WHERE modeltypes_formats.modeltype_id=models.modeltype_id" .
                        " AND models.id=?;");
  if (!$stmt->execute(array($_REQUEST['model']))) {
    die('Invalid query: ' . error_database());
  }
  $formats=$stmt->fetchAll(PDO::FETCH_COLUMN, 0);
  $stmt->closeCursor();

  if (count($formats) == 0) {
    allSites();
  } else {
    // 2. Find all formats that are in world
    $stmt = $pdo->prepare("SELECT DISTINCT format_id FROM inputs, dbfiles, machines" .
                          " WHERE inputs.site_id=${earth} AND inputs.id=dbfiles.container_id" .
                          " AND dbfiles.container_type='Input' and dbfiles.machine_id=machines.id" .
                          " AND machines.hostname=? GROUP BY format_id;");
    if (!$stmt->execute(array($_REQUEST['host']))) {
      die('Invalid query: ' . error_database());
    }
    $formats=array_diff($formats, $stmt->fetchAll(PDO::FETCH_COLUMN, 0));
    $stmt->closeCursor();

    // 3. Get list of all sites, formats
    $stmt = $pdo->prepare("SELECT DISTINCT sites.id, sites.sitename, sites.city, sites.country, ST_X(ST_CENTROID(sites.geometry)) AS lon, ST_Y(ST_CENTROID(sites.geometry)) AS lat, format_id FROM sites, inputs, dbfiles, machines" .
                          " WHERE sites.id=inputs.site_id AND inputs.id=dbfiles.container_id" .
                          " AND dbfiles.container_type='Input' and dbfiles.machine_id=machines.id" .
                          " AND machines.hostname=? GROUP BY sites.id, format_id;");
    if (!$stmt->execute(array($_REQUEST['host']))) {
      die('Invalid query: ' . error_database());
    }

    // 4. Loop over result, collecting formats used
    $sites = array();
    while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
      if (in_array($row['format_id'], $formats)) {
        if (array_key_exists($row['id'], $sites)) {
          $sites[$row['id']]['format_id'][] = $row['format_id'];
        } else {
          $row['format_id'] = array($row['format_id']);
          $sites[$row['id']] = $row;
        }
      }
    } 
    $stmt->closeCursor();

    foreach($sites as $site) {
      if (count(array_diff($formats, $site['format_id'])) == 0) {
        addNode($site);
      }
    }
  }
} else {
  allSites();
}

echo $dom->saveXML();
close_database();

function allSites() {
  global $pdo;

  // Select all the rows in the markers table
  $result = $pdo->query("SELECT id, sitename, city, country, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites;");
  if (!$result) {
    die('Invalid query: ' . error_database());
  } 

  // Iterate through the rows, adding XML nodes for each
  while ($row = @$result->fetch(PDO::FETCH_ASSOC)) { 
    addNode($row);
  } 
}

function addNode($row) {
  global $dom, $parnode;

  $node = $dom->createElement("marker");
  $newnode = $parnode->appendChild($node);   
  $newnode->setAttribute("siteid",$row['id']);
  $newnode->setAttribute("city", $row['city']);
  $newnode->setAttribute("country", $row['country']); 
  $newnode->setAttribute("lat", $row['lat']); 
  $newnode->setAttribute("lon", $row['lon']);
  if ($row['sitename'] != "") {
    $newnode->setAttribute("sitename", $row['sitename']);
  } else {  
    $newnode->setAttribute("sitename", $row['id'] . " - " . $row['city']);
  }
}
?>
