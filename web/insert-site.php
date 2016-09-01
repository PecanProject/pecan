<?php
  require("common.php");
  open_database();
  if ($autentication) {
    if (!check_login()) {
      header( "Location: index.php");
      close_database();
      exit;
    }
    if (get_page_access_level() > $min_run_level) {
      header( "Location: history.php");
      close_database();
      exit;
    }
  }


#parameters
$offline=isset($_REQUEST['offline']);


$sitename = "";
if (isset($_REQUEST['sitename'])) {
  $sitename = $_REQUEST['sitename'];
}

$city = "";
if (isset($_REQUEST['city'])) {
  $city = $_REQUEST['city'];
}

$state = "";
if (isset($_REQUEST['state'])) {
  $state = $_REQUEST['state'];
}

$country = "";
if (isset($_REQUEST['country'])) {
  $country = $_REQUEST['country'];
}

$mat = "";
if (isset($_REQUEST['mat'])) {
  $mat = $_REQUEST['mat'];
}

$map = "";
if (isset($_REQUEST['map'])) {
  $map = $_REQUEST['map'];
}

$soil = "";
if (isset($_REQUEST['soil'])) {
  $soil = $_REQUEST['soil'];
}






$greenhouse = "";
if (isset($_REQUEST['greenhouse'])) {
  $greenhouse = $_REQUEST['greenhouse'];
}

$user_id = "";
if (isset($_REQUEST['user_id'])) {
  $user_id = $_REQUEST['user_id'];
}

$local_time = "";
if (isset($_REQUEST['local_time'])) {
  $local_time = $_REQUEST['local_time'];
}

$sand_percentage = "";
if (isset($_REQUEST['sand_percentage'])) {
  $sand_percentage = $_REQUEST['sand_percentage'];
}

$clay_percentage = "";
if (isset($_REQUEST['clay_percentage'])) {
  $clay_percentage = $_REQUEST['clay_percentage'];
}

$geometry = "";
if (isset($_REQUEST['geometry'])) {
  $geometry = $_REQUEST['geometry'];
}

// Insert new site
$query = "INSERT INTO sites (city, state, country, mat, map, soil, created_at, updated_at,
                             sitename, greenhouse, user_id, local_time, sand_percentage,
                             clay_percentage, geometry)
                 VALUES
                            (:city, :state, :country, :mat, :map, :soil, NOW(), NOW(),
                             :sitename, :greenhouse, :user_id, :local_time, :sand_percentage,
                             :clay_percentage, :geometry)";

$stmt = $pdo->prepare($query);

$stmt->bindParam(':city', $city, PDO::PARAM_STR);
$stmt->bindParam(':state', $state, PDO::PARAM_STR);
$stmt->bindParam(':country', $country, PDO::PARAM_STR);
$stmt->bindParam(':mat', $mat, PDO::PARAM_INT);
$stmt->bindParam(':map', $map, PDO::PARAM_INT);
$stmt->bindParam(':soil', $soil, PDO::PARAM_STR);
$stmt->bindParam(':sitename', $sitename, PDO::PARAM_STR);
$stmt->bindParam(':greenhouse', $greenhouse, PDO::PARAM_STR);
$stmt->bindParam(':user_id', $user_id, PDO::PARAM_STR);
$stmt->bindParam(':local_time', $localtime, PDO::PARAM_STR);
$stmt->bindParam(':sand_percentage', $sand_percentage, PDO::PARAM_INT);
$stmt->bindParam(':clay_percentage', $clay_percentage, PDO::PARAM_INT);
$stmt->bindParam(':geometry', $geometry, PDO::PARAM_INT);

if ($stmt->execute() === FALSE) {
  die('Can\'t insert new site query: ' . error_database());
}
//Need to return structure of new marker to add to markersArray

?>
