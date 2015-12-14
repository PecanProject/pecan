<?php

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

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$dom->formatOutput = true;
$node = $dom->createElement("workflows");
$parnode = $dom->appendChild($node);

// get run information
$query = "SELECT workflows.id, workflows.folder, workflows.start_date, workflows.end_date, workflows.started_at, workflows.finished_at, " .
         "CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) AS sitename, " .
         "CONCAT(coalesce(models.model_name, ''), ' ', coalesce(models.revision, '')) AS modelname, modeltypes.name " .
         "FROM workflows " .
         "LEFT OUTER JOIN sites on workflows.site_id=sites.id " .
         "LEFT OUTER JOIN models on workflows.model_id=models.id " .
         "LEFT OUTER JOIN modeltypes on models.modeltype_id=modeltypes.id ";

$where = "";

if (isset($_REQUEST["onlyme"])) {
  if ($where != "") $where .= "AND ";
  $where .= "workflows.user_id=:userid ";
}

if (isset($_REQUEST["search"])) {
  if ($where != "") $where .= "AND ";
  $where .= "(CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) LIKE :search " .
         "OR CONCAT(coalesce(models.model_name, ''), ' ', coalesce(models.revision, '')) LIKE :search " .
         "OR modeltypes.name LIKE :search " .
         "OR workflows.notes LIKE :search) ";
}

if ($where != "") {
  $query .= "WHERE ${where}";
}
$query .= "ORDER BY workflows.id DESC";

$time = microtime(true);
$stmt=$pdo->prepare($query);
if (isset($_REQUEST["onlyme"])) {
  $userid=get_userid();
  $stmt->bindParam(':userid', $userid, PDO::PARAM_INT);
}
if (isset($_REQUEST["search"])) {
  $search = '%'. $_REQUEST['search'] . '%';
  $stmt->bindParam(':search', $search, PDO::PARAM_STR);
}
if ($stmt->execute() === FALSE) {
  die('Invalid query: ' . error_database());
}

$time = microtime(true);
$rows = 0;
$show = 0;
while ($row = $stmt->fetch(PDO::FETCH_ASSOC)) {
  $rows++;
  $status="";
  if (file_exists($row['folder'] . DIRECTORY_SEPARATOR . "STATUS")) {
    $statusfile=file($row['folder'] . DIRECTORY_SEPARATOR . "STATUS");
    foreach ($statusfile as $line) {
      $data = explode("\t", $line);
      if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
        $status = "ERROR";
      }
    }
  } else {
    if (!isset($_REQUEST["show_unknown"])) {
      continue;
    }
    $status = "UNKNOWN";
  }
  if (($status == "") && ($row['finished_at'] == "")) {
    $status = "RUNNING";
  }
  if ($status == "") {
    $status = "DONE";
  }

  $node = $dom->createElement("workflow");
  $newnode = $parnode->appendChild($node);   
  $newnode->setAttribute("id",$row['id']);
  $newnode->setAttribute("status",$status);
  $newnode->setAttribute("sitename",$row['sitename']);
  $newnode->setAttribute("modelname", $row['modelname']);
  $newnode->setAttribute("name", $row['name']);
  $newnode->setAttribute("start_date", $row['start_date']);
  $newnode->setAttribute("end_date", $row['end_date']);
  $newnode->setAttribute("started_at", $row['started_at']);
  $newnode->setAttribute("finished_at", $row['finished_at']);
  $show++;
}

$time = (microtime(true) - $time);
$node = $dom->createElement("stats");
$newnode = $parnode->appendChild($node);
$newnode->setAttribute("time", $time);
$newnode->setAttribute("rows", $rows);
$newnode->setAttribute("returned", $show);
$newnode->setAttribute("average", ($time / $rows));

//echo htmlspecialchars($dom->saveXML());
echo $dom->saveXML();

close_database();
?>
