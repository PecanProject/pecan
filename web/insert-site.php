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


#debug_to_console( "Test");
#debug_to_console( "Sitename " . $sitename);

//varchar 255
$city = "";
if (isset($_REQUEST['city'])) {
  $city = $_REQUEST['city'];
}

//varchar 255
$state = "";
if (isset($_REQUEST['state'])) {
  $state = $_REQUEST['state'];
}

//varchar 255
$country = "";
if (isset($_REQUEST['country'])) {
  $country = $_REQUEST['country'];
}

//numeric(4,2)(C)
$mat = "";
if (isset($_REQUEST['mat'])) {
  $mat = $_REQUEST['mat'];
}

//integer (mm)
$map = "";
if (isset($_REQUEST['map'])) {
  $map = $_REQUEST['map'];
}

//varchar 255
$soil = "test-soil";
if (isset($_REQUEST['soil'])) {
  $soil = $_REQUEST['soil'];
}

// SOM is deprecated

// text
$notes = "";
if (isset($_REQUEST['notes'])) {
  $notes = $_REQUEST['notes'];
}

// text
$soilnotes = "";
if (isset($_REQUEST['soilnotes'])) {
  $soilnotes = $_REQUEST['soilnotes'];
}

// created_at and updated_at both use timestamps (NOW()) is usual function call

// varchar 255
$sitename = "";
if (isset($_REQUEST['sitename'])) {
  $sitename = $_REQUEST['sitename'];
}

// boolean = study conducted in a field which is 0 or greenhouse, pot, or growth chamber which is 1
$greenhouse = (isset($_REQUEST['greenhouse'])) ? 1 : 0;

// bigint, but needs to be a foreign key into the users table
// not currently used
$user_id = get_userid();

// numeric(9,5)
$sand_percentage = "";
if (isset($_REQUEST['pctsand'])) {
  $sand_percentage = $_REQUEST['pctsand'];
}

// numeric(9,5)
$clay_percentage = "";
if (isset($_REQUEST['pctclay'])) {
  $clay_percentage = $_REQUEST['pctclay'];
}

// part of geometry field (geometry(geometryZ, 4326)
$lat = "";
if (isset($_REQUEST['lat'])) {
  $lat = $_REQUEST['lat'];
}

// part of geometry field (geometry(geometryZ, 4326)
$long = "";
if (isset($_REQUEST['long'])) {
  $long = $_REQUEST['long'];
}

// text
$time_zone = "";
if (isset($_REQUEST['time_zone'])) {
  $time_zone = $_REQUEST['time_zone'];
}

// might be part of geometry field but cant get it accepted with the 4326 SRID geometry schema
$elevation = "";
if (isset($_REQUEST['elevation'])) {
  $elevation = $_REQUEST['elevation'];
}

// Insert new site
if ($user_id != -1) {
$query = "INSERT INTO sites (city, state, country, mat, map, soil, notes, soilnotes, created_at, updated_at,
                             sitename, greenhouse, user_id, time_zone, sand_pct,
                             clay_pct, geometry)
                 VALUES
                            (:city, :state, :country, :mat, :map, :soil, :notes, :soilnotes, NOW(), NOW(),
                             :sitename, :greenhouse, :user_id, :time_zone, :sand_percentage,
                             :clay_percentage, ST_SetSRID(ST_MakePoint(:long, :lat, :elevation), 4326))
		 RETURNING id";
} else {
$query = "INSERT INTO sites (city, state, country, mat, map, soil, notes, soilnotes, created_at, updated_at,
                             sitename, greenhouse, time_zone, sand_pct,
                             clay_pct, geometry)
                 VALUES
                            (:city, :state, :country, :mat, :map, :soil, :notes, :soilnotes, NOW(), NOW(),
                             :sitename, :greenhouse, :time_zone, :sand_percentage,
                             :clay_percentage, ST_SetSRID(ST_MakePoint(:long, :lat, :elevation), 4326))
		 RETURNING id";

}
$stmt = $pdo->prepare($query);

$stmt->bindParam(':city', $city, PDO::PARAM_STR);
$stmt->bindParam(':state', $state, PDO::PARAM_STR);
$stmt->bindParam(':country', $country, PDO::PARAM_STR);
$stmt->bindParam(':mat', $mat, PDO::PARAM_STR);
$stmt->bindParam(':map', $map, PDO::PARAM_INT); //integer
$stmt->bindParam(':elevation', $elevation, PDO::PARAM_STR);
$stmt->bindParam(':soil', $soil, PDO::PARAM_STR);
$stmt->bindParam(':notes', $notes, PDO::PARAM_STR);
$stmt->bindParam(':soilnotes', $soilnotes, PDO::PARAM_STR);
$stmt->bindParam(':sitename', $sitename, PDO::PARAM_STR);
$stmt->bindParam(':greenhouse', $greenhouse, PDO::PARAM_BOOL);
$stmt->bindParam(':time_zone', $time_zone, PDO::PARAM_STR);
$stmt->bindParam(':sand_percentage', $sand_percentage, PDO::PARAM_STR);
$stmt->bindParam(':clay_percentage', $clay_percentage, PDO::PARAM_STR);
$stmt->bindParam(':lat', $lat, PDO::PARAM_STR); //decimal
$stmt->bindParam(':long', $long, PDO::PARAM_STR); //decimal

if ($user_id != -1) {
  $stmt->bindParam(':user_id', $user_id, PDO::PARAM_INT);
}

if ($stmt->execute() === FALSE) {
  die('Can\'t insert new site query: ' . error_database());
  
} else {
  //Need to return structure of new marker to add to markersArray
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  $dom = new DOMDocument("1.0");
  $dom->formatOutput = true;
  $root = $dom->appendChild($dom->createElement("info"));

  $parnode = $root->appendChild($dom->createElement("markers"));

  $node = $dom->createElement("marker");
  $newnode = $parnode->appendChild($node);
  $newnode->setAttribute("siteid", $result["id"]);
  $newnode->setAttribute("city", $city);
  $newnode->setAttribute("country", $country);
  $newnode->setAttribute("lat", $lat);
  $newnode->setAttribute("lon", $long);
  $newnode->setAttribute("sitename", $sitename);
  echo $dom->saveXML();

}

?>
