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
$city = "test-city";
if (isset($_REQUEST['txtcity'])) {
  $city = $_REQUEST['txtcity'];
}

//varchar 255
$state = "test-state";
if (isset($_REQUEST['txtstate'])) {
  $state = $_REQUEST['txtstate'];
}

//varchar 255
$country = "test-country";
if (isset($_REQUEST['txtcountry'])) {
  $country = $_REQUEST['txtcountry'];
}

//numeric(4,2)(C)
$mat = "26.45";
if (isset($_REQUEST['txtmat'])) {
  $mat = $_REQUEST['txtmat'];
}

//integer (mm)
$map = "45";
if (isset($_REQUEST['txtmap'])) {
  $map = $_REQUEST['txtmap'];
}

//varchar 255
$soil = "test-soil";
if (isset($_REQUEST['txtsoil'])) {
  $soil = $_REQUEST['txtsoil'];
}

// SOM is deprecated

// text
$notes = "test-notes";
if (isset($_REQUEST['txtnotes'])) {
  $notes = $_REQUEST['txtnotes'];
}

// text
$soilnotes = "test-soil-notes";
if (isset($_REQUEST['txtsoilnotes'])) {
  $soilnotes = $_REQUEST['txtsoilnotes'];
}

// created_at and updated_at both use timestamps (NOW()) is usual function call

// varchar 255
$sitename = "test-site";
if (isset($_REQUEST['txtsitename'])) {
  $sitename = $_REQUEST['txtsitename'];
}

// boolean = study conducted in a field which is 0 or greenhouse, pot, or growth chamber which is 1
$greenhouse = isset($_REQUEST['txtgreenhouse']);

// bigint, but needs to be a foreign key into the users table
// not currently used
$user_id = "1";
if (isset($_REQUEST['txtuser_id'])) {
  $user_id = $_REQUEST['txtuser_id'];
}


// numeric(9,5)
$sand_percentage = "97.307";
if (isset($_REQUEST['txtpctsand'])) {
  $sand_percentage = $_REQUEST['txtpctsand'];
}

// numeric(9,5)
$clay_percentage = "2.1056";
if (isset($_REQUEST['txtpctclay'])) {
  $clay_percentage = $_REQUEST['txtpctclay'];
}

// part of geometry field (geometry(geometryZ, 4326)
$lat = "-88.2826";
if (isset($_REQUEST['txtlat'])) {
  $lat = $_REQUEST['txtlat'];
}

// part of geometry field (geometry(geometryZ, 4326)
$long = "40.0703";
if (isset($_REQUEST['txtlong'])) {
  $long = $_REQUEST['txtlong'];
}

// text
$time_zone = "Central";
if (isset($_REQUEST['txttime_zone'])) {
  $time_zone = $_REQUEST['txttime_zone'];
}

// might be part of geometry field but cant get it accepted with the 4326 SRID geometry schema
$elevation = "0.0";
if (isset($_REQUEST['txtelevation'])) {
  $elevation = $_REQUEST['txtelevation'];
}

// Insert new site
$query = "INSERT INTO sites (city, state, country, mat, map, soil, notes, soilnotes, created_at, updated_at,
                             sitename, greenhouse, user_id, time_zone, sand_pct,
                             clay_pct, geometry)
                 VALUES
                            (:city, :state, :country, :mat, :map, :soil, :notes, :soilnotes, NOW(), NOW(),
                             :sitename, :greenhouse, :user_id, :time_zone, :sand_percentage,
                             :clay_percentage, ST_SetSRID(ST_MakePoint(:long, :lat, :elevation), 4326))
		 RETURNING id";
//$query = "INSERT INTO sites (city, state, country, mat, map, soil, notes, soilnotes, created_at, updated_at,
//                             sitename, greenhouse, user_id, time_zone, sand_pct,
//                             clay_pct, geometry)
//                 VALUES
//                            (:city, :state, :country, :mat, :map, :soil, :notes, :soilnotes, NOW(), NOW(),
//                             :sitename, :greenhouse, :user_id, :time_zone, :sand_percentage,
//                             :clay_percentage, ST_Force3D(ST_SetSRID(ST_MakePoint(:long, :lat),4326)))
//		 RETURNING id";

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
$stmt->bindParam(':user_id', $user_id, PDO::PARAM_INT);
$stmt->bindParam(':time_zone', $time_zone, PDO::PARAM_STR);
$stmt->bindParam(':sand_percentage', $sand_percentage, PDO::PARAM_STR);
$stmt->bindParam(':clay_percentage', $clay_percentage, PDO::PARAM_STR);
$stmt->bindParam(':lat', $lat, PDO::PARAM_STR); //decimal
$stmt->bindParam(':long', $long, PDO::PARAM_STR); //decimal

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
  //echo htmlspecialchars($dom->saveXML());
  return $dom;
}

?>
