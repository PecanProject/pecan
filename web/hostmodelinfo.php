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
    header('HTTP/1.1 403 Unauthorized');
    close_database();
    exit;
  }
  if (get_page_acccess_level() > $min_run_level) {
    header('HTTP/1.1 403 Unauthorized');
    close_database();
    exit;
  }
}

// what site is earth
$earth = 1118;

// some constants for met formats, this is a list of all models that
// implement a CF to model code, with the appropriate output format.
$met = array('ED2'      => 12,
             'SIPNET'   => 24,
             'BIOCRO'   => 28,
             'CF'       => 33,
             'DALEC'    => 1000000002,
             'LINKAGES' => 1000000003,
             'MAESPA'   => 1000000016,
             'LPJGUESS' => 1000000017);

$host = isset($_REQUEST['host']) ? $_REQUEST['host'] : "";
$model = isset($_REQUEST['model']) ? $_REQUEST['model'] : "";
$sitegroup = isset($_REQUEST['sitegroup']) ? $_REQUEST['sitegroup'] : "";
if ($sitegroup == "-1") $sitegroup = "";

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$dom->formatOutput = true;
$root = $dom->appendChild($dom->createElement("info"));

// get information
get_hosts();
get_models();
get_sites();

echo $dom->saveXML();

close_database();

// ----------------------------------------------------------------------
// SITES
// ----------------------------------------------------------------------
function get_hosts() {
  global $pdo, $dom, $root, $hostlist;
  global $model;

  $parnode = $root->appendChild($dom->createElement("hosts"));

  // check for hosts
  $query = "SELECT machines.* FROM machines";
  $subs = array();
  if ($model) {
    $query .= " INNER JOIN dbfiles ON machines.id = dbfiles.machine_id";
    $query .= " INNER JOIN models ON dbfiles.container_id = models.id";
    $query .= " WHERE dbfiles.container_type='Model' AND models.id=?";
    $subs[] = $model;
  }

  // Select all the rows
  $query .= " ORDER BY machines.hostname";
  $stmt = $pdo->prepare($query);
  if (!$stmt->execute($subs)) {
    die('Invalid query: ' . error_database());
  }
    
  // Iterate through the rows, adding XML nodes for each
  while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
    if (array_key_exists($row['hostname'], $hostlist)) {
      $name = $hostlist[$row['hostname']]['displayname'] ?: $row['hostname'];
      $node = $dom->createElement("host");
      $newnode = $parnode->appendChild($node);   
      $newnode->setAttribute("id", $row['id']);
      $newnode->setAttribute("hostname", $row['hostname']);
      $newnode->setAttribute("displayname", $name);
    }
  } 
  $stmt->closeCursor();
}

// ----------------------------------------------------------------------
// MODELS
// ----------------------------------------------------------------------
function get_models() {
  global $pdo, $dom, $root, $hostlist;
  global $host;

  $parnode = $root->appendChild($dom->createElement("models"));

  // check for models
  $query = "SELECT models.*, machines.hostname FROM models";
  $query .= " INNER JOIN dbfiles ON models.id=dbfiles.container_id";
  $query .= " INNER JOIN machines ON dbfiles.machine_id=machines.id";
  $query .= " WHERE dbfiles.container_type='Model'";
  $subs = array();
  if ($host) {
    $query .= " AND machines.hostname=?";
    $subs[] = $host;
  }
  $query .= " ORDER BY models.model_name DESC, models.revision DESC";

  // Select all the rows in the models table
  $stmt = $pdo->prepare($query);
  if (!$stmt->execute($subs)) {
    die('Invalid query: ' . error_database());
  }
    
  // Iterate through the rows, adding XML nodes for each
  while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) { 
    if (array_key_exists($row['hostname'], $hostlist)) {
      $node = $dom->createElement("model");
      $newnode = $parnode->appendChild($node);   
      $newnode->setAttribute("id",$row['id']);
      $newnode->setAttribute("name", $row['model_name']);
      $newnode->setAttribute("revision", $row['revision']);  
    }
  } 
  $stmt->closeCursor();
}

// ----------------------------------------------------------------------
// SITES
// ----------------------------------------------------------------------
function get_sites() {
  global $pdo, $dom, $root, $hostlist;
  global $earth, $met, $host, $model, $sitegroup;

  $parnode = $root->appendChild($dom->createElement("markers"));

  // 1. Get a list of all sites we have
  $subs = array();
  $where = "";
  $query = "SELECT sites.id, sites.sitename, sites.city, sites.country, ST_X(ST_CENTROID(sites.geometry)) AS lon, ST_Y(ST_CENTROID(sites.geometry)) AS lat FROM sites";
  if ($sitegroup) {
    $query .= " INNER JOIN sitegroups_sites ON sitegroups_sites.site_id=sites.id";
    $where .= $where == "" ? " WHERE" : " AND";
    $where .= " sitegroups_sites.sitegroup_id=?";
    $subs[] = $sitegroup;
  }
  $query .= $where;
  $stmt = $pdo->prepare($query);
  if (!$stmt->execute($subs)) {
    die('Invalid query: ' . error_database());
  }
  $sites = array();
  while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
    $row['format_id'] = array();
    $sites[$row['id']] = $row;
  }

  // in case of model we will need to filter those sites that can never be run
  if ($model) {
    // 1. Get list of all formats for each site
    $subs = array();
    $query = "SELECT DISTINCT sites.id, format_id FROM sites";
    $query .= " INNER JOIN inputs ON sites.id=inputs.site_id";
    $query .= " INNER JOIN dbfiles ON inputs.id=dbfiles.container_id";
    $where = " AND dbfiles.container_type='Input'";
    if ($sitegroup) {
      $query .= " INNER JOIN sitegroups_sites ON sitegroups_sites.site_id=sites.id";
      $where .= " AND sitegroups_sites.sitegroup_id=?";
      $subs[] = $sitegroup;
    }
    if ($host) {
      $query .= " INNER JOIN machines ON dbfiles.machine_id=machines.id";
      $where .= " AND machines.hostname=?";
      $subs[] = $host;
    }
    $query .= $where . " GROUP BY sites.id, format_id;";
    $stmt = $pdo->prepare($query);
    if (!$stmt->execute($subs)) {
      die('Invalid query: ' . error_database());
    }
    while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
      $sites[$row['id']]['format_id'][] = $row['format_id'];
    } 
    $stmt->closeCursor();

    // 2. Find all formats that are in world
    $subs = array();
    $query = "SELECT DISTINCT format_id FROM inputs";
    $query .= " INNER JOIN dbfiles ON inputs.id=dbfiles.container_id";
    $where = " WHERE inputs.site_id={$earth} AND dbfiles.container_type='Input'";
    if ($host) {
      $query .= " INNER JOIN machines ON dbfiles.machine_id=machines.id";
      $where .= " AND machines.hostname=?";
      $subs[] = $host;
    }
    $query .= $where . " GROUP BY format_id;";
    $stmt = $pdo->prepare($query);
    if (!$stmt->execute($subs)) {
      die('Invalid query: ' . error_database());
    }
    while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
      foreach($sites as &$site) {
        $site['format_id'][] = $row['format_id'];
      }
      unset($site);
    } 
    $stmt->closeCursor();

    // 3. Find all conversions possible
    if (isset($_REQUEST['conversion'])) {

      // Check for Download -> CF
      foreach($sites as &$site) {
        if (!in_array($met['CF'], $site['format_id'])) {
          $site['format_id'][] = $met['CF'];
        }
      }
      unset($site);

      // Check for CF -> model
      $stmt = $pdo->prepare("SELECT modeltypes.name FROM modeltypes, models" .
                            " WHERE modeltypes.id=models.modeltype_id" .
                            " AND models.id=?;");
      if (!$stmt->execute(array($model))) {
        die('Invalid query: ' . error_database());
      }
      $modeltypes=$stmt->fetchAll(PDO::FETCH_COLUMN, 0);
      $stmt->closeCursor();
      foreach($sites as &$site) {
        if (in_array($met['CF'], $site['format_id'])) {
          foreach($modeltypes as $mt) {
            if (array_key_exists($mt, $met)) {
              $site['format_id'][] = $met[$mt];
            }
          }
        }
      }
      unset($site);
    }

    // 4. Get list of all formats needed for model
    $stmt = $pdo->prepare("SELECT format_id FROM modeltypes_formats, models" .
                          " WHERE modeltypes_formats.modeltype_id=models.modeltype_id" .
                          " AND modeltypes_formats.required = true".
                          " AND models.id=?;");

    if (!$stmt->execute(array($model))) {
      die('Invalid query: ' . error_database());
    }
    $formats=$stmt->fetchAll(PDO::FETCH_COLUMN, 0);
    $stmt->closeCursor();

    // 5. Filter all sites that have missing formats
    $filtered = array();
    foreach($sites as $site) {
      if (count(array_diff($formats, $site['format_id'])) == 0) {
        $filtered[] = $site;
      }
    }
    unset($site);
    $sites = $filtered;
  }

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
}
?>
