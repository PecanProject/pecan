<?php
  require("common.php");
  open_database();
  if ($authentication) {
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


function debug_to_console( $data) {

	if ( is_array( $data ) )
		$output = "<script> console.log( 'Debug Objects: " . implode( ',', $data) . "');</script>";
	else
		$output = "<script> console.log( 'Debug Objects: " .  $data . "' );</script>";
	echo $output;
}




#parameters
$offline=isset($_REQUEST['offline']);


$sitename = "test-site";
if (isset($_POST['txtsitename'])) {
  $sitename = $_POST['txtsitename'];
}
debug_to_console( "Test");
debug_to_console( "Sitename " . $sitename);

$city = "test-city";
if (isset($_POST['txtcity'])) {
  $city = $_POST['txtcity'];
}

$state = "test-state";
if (isset($_POST['txtstate'])) {
  $state = $_POST['txtstate'];
}

$country = "test-country";
if (isset($_POST['txtcountry'])) {
  $country = $_POST['txtcountry'];
}

$mat = "1";
if (isset($_POST['mat'])) {
  $mat = $_POST['mat'];
}

$map = "1";
if (isset($_POST['map'])) {
  $map = $_POST['map'];
}

$soil = "test-soil";
if (isset($_POST['soil'])) {
  $soil = $_POST['soil'];
}



$greenhouse = "true";
if (isset($_REQUEST['greenhouse'])) {
  $greenhouse = $_REQUEST['greenhouse'];
}

$user_id = "1";
if (isset($_REQUEST['user_id'])) {
  $user_id = $_REQUEST['user_id'];
}

$time_zone = "Central";
if (isset($_REQUEST['time_zone'])) {
  $time_zone = $_REQUEST['time_zone'];
}

$sand_percentage = "1";
if (isset($_REQUEST['sand_percentage'])) {
  $sand_percentage = $_REQUEST['sand_percentage'];
}

$clay_percentage = "2";
if (isset($_REQUEST['clay_percentage'])) {
  $clay_percentage = $_REQUEST['clay_percentage'];
}

$geometry = "90";
if (isset($_REQUEST['geometry'])) {
  $geometry = $_REQUEST['geometry'];
}

// Insert new site
$query = "INSERT INTO sites (city, state, country, mat, map, soil, created_at, updated_at,
                             sitename, greenhouse, user_id, time_zone, sand_pct,
                             clay_pct, geometry)
                 VALUES
                            (:city, :state, :country, :mat, :map, :soil, NOW(), NOW(),
                             :sitename, :greenhouse, :user_id, :time_zone, :sand_percentage,
                             :clay_percentage, ST_GeomFromText('POINT(:long, :lat)',4326))";

$stmt = $pdo->prepare($query);

$stmt->bindParam(':city', $city, PDO::PARAM_STR);
$stmt->bindParam(':state', $state, PDO::PARAM_STR);
$stmt->bindParam(':country', $country, PDO::PARAM_STR);
$stmt->bindParam(':mat', $mat, PDO::PARAM_INT);
$stmt->bindParam(':map', $map, PDO::PARAM_INT);
$stmt->bindParam(':lat', $lat, PDO::PARAM_INT);
$stmt->bindParam(':long', $long, PDO::PARAM_INT);
$stmt->bindParam(':soil', $soil, PDO::PARAM_STR);
$stmt->bindParam(':sitename', $sitename, PDO::PARAM_STR);
$stmt->bindParam(':greenhouse', $greenhouse, PDO::PARAM_STR);
$stmt->bindParam(':user_id', $user_id, PDO::PARAM_INT);
$stmt->bindParam(':time_zone', $time_zone, PDO::PARAM_STR);
$stmt->bindParam(':sand_percentage', $sand_percentage, PDO::PARAM_INT);
$stmt->bindParam(':clay_percentage', $clay_percentage, PDO::PARAM_INT);

if ($stmt->execute() === FALSE) {
  die('Can\'t insert new site query: ' . error_database());
}
//Need to return structure of new marker to add to markersArray

?>
