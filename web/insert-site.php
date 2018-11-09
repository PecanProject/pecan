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

  // varchar 255
  if (!isset($_REQUEST['newsite'])) {
    die("Missing newsite");  
  }
  $sitename = $_REQUEST['newsite'];

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

  //text
  $timezone = "";
  if (isset($_REQUEST['timezone'])) {
    $timezone = $_REQUEST['timezone'];
  }

  // part of geometry field (geometry(geometryZ, 4326)
  $lat = "";
  if (isset($_REQUEST['lat'])) {
    $lat = $_REQUEST['lat'];
  }

  // part of geometry field (geometry(geometryZ, 4326)
  $lon = "";
  if (isset($_REQUEST['lon'])) {
    $lon = $_REQUEST['lon'];
  }

  // might be part of geometry field but cant get it accepted with the 4326 SRID geometry schema
  $elevation = "";
  if (isset($_REQUEST['elevation'])) {
    $elevation = $_REQUEST['elevation'];
  }

  //integer (mm)
  $percipitation = "";
  if (isset($_REQUEST['percipitation'])) {
    $percipitation = $_REQUEST['percipitation'];
  }

  //numeric(4,2)(C)
  $temperature = "";
  if (isset($_REQUEST['temperature'])) {
    $temperature = $_REQUEST['temperature'];
  }

  // numeric(9,5)
  $clay_percentage = "";
  if (isset($_REQUEST['clay'])) {
    $clay_percentage = $_REQUEST['clay'];
  }

  // numeric(9,5)
  $sand_percentage = "";
  if (isset($_REQUEST['sand'])) {
    $sand_percentage = $_REQUEST['sand'];
  }

  // text
  $soilnotes = "";
  if (isset($_REQUEST['soilnotes'])) {
    $soilnotes = $_REQUEST['soilnotes'];
  }

  // text
  $notes = "";
  if (isset($_REQUEST['notes'])) {
    $notes = $_REQUEST['notes'];
  }

  // boolean = study conducted in a field which is 0 or greenhouse, pot, or growth chamber which is 1
  $greenhouse = (isset($_REQUEST['greenhouse'])) ? 1 : 0;

  // bigint, but needs to be a foreign key into the users table
  // not currently used
  $user_id = get_userid();

  // text
  $time_zone = "";
  if (isset($_REQUEST['time_zone'])) {
    $time_zone = $_REQUEST['time_zone'];
  }


  // Insert new site
  $query = "INSERT INTO sites (sitename, city, state, country, time_zone, geometry, mat, map, clay_pct, sand_pct, soilnotes, notes, greenhouse";
  if ($user_id != -1) {
    $query = "$query, user_id";
  }
  $query = "$query)";

  $query = "$query VALUES (:sitename, :city, :state, :country, :timezone, ST_SetSRID(ST_MakePoint(:lon, :lat, :elevation), 4326), :temperature, :percipitation, :clay_percentage, :sand_percentage, :soilnotes, :notes, :greenhouse";
  if ($user_id != -1) {
    $query = "$query, :user_id";
  }
  $query = "$query) RETURNING id, sitename";

  $stmt = $pdo->prepare($query);

  $stmt->bindParam(':sitename', $sitename, PDO::PARAM_STR);
  $stmt->bindParam(':city', $city, PDO::PARAM_STR);
  $stmt->bindParam(':state', $state, PDO::PARAM_STR);
  $stmt->bindParam(':country', $country, PDO::PARAM_STR);
  $stmt->bindParam(':timezone', $timezone, PDO::PARAM_STR);
  $stmt->bindParam(':lat', $lat, PDO::PARAM_STR); //decimal
  $stmt->bindParam(':lon', $lon, PDO::PARAM_STR); //decimal
  $stmt->bindParam(':elevation', $elevation, PDO::PARAM_STR);

  $stmt->bindParam(':temperature', $temperature, PDO::PARAM_STR);
  $stmt->bindParam(':percipitation', $percipitation, PDO::PARAM_INT); //integer

  $stmt->bindParam(':clay_percentage', $clay_percentage, PDO::PARAM_STR);
  $stmt->bindParam(':sand_percentage', $sand_percentage, PDO::PARAM_STR);
  $stmt->bindParam(':soilnotes', $soilnotes, PDO::PARAM_STR);

  $stmt->bindParam(':notes', $notes, PDO::PARAM_STR);
  $stmt->bindParam(':greenhouse', $greenhouse, PDO::PARAM_BOOL);

  //$stmt->bindParam(':soil', $soil, PDO::PARAM_STR);

  if ($user_id != -1) {
    $stmt->bindParam(':user_id', $user_id, PDO::PARAM_INT);
  }

  if ($stmt->execute() === FALSE) {
    die('Can\'t insert new site query: ' . error_database());
    
  } else {
    $result = $stmt->fetch(PDO::FETCH_ASSOC);
    echo "${result['id']} ${result['sitename']}";
  }
?>
