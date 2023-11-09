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

  // sitegroup
  $sitegroupid = $_REQUEST['newsitegroupid'] ?: "-1";

  //varchar 255
  $city = $_REQUEST['city'] ?: "";

  //varchar 255
  $state = $_REQUEST['state'] ?: "";

  //varchar 255
  $country = $_REQUEST['country'] ?: "";

  //text
  $time_zone = $_REQUEST['time_zone'] ?: "";

  // part of geometry field (geometry(geometryZ, 4326)
  if (!isset($_REQUEST['lat'])) {
    die("Missing lat");
  }
  $lat = $_REQUEST['lat'];

  // part of geometry field (geometry(geometryZ, 4326)
  if (!isset($_REQUEST['lon'])) {
    die("Missing lon");
  }
  $lon = $_REQUEST['lon'];

  // might be part of geometry field but cant get it accepted with the 4326 SRID geometry schema
  $elevation = $_REQUEST['elevation'] ?: "0";

  //integer (mm)
  $percipitation = $_REQUEST['percipitation'] ?: "";

  //numeric(4,2)(C)
  $temperature = $_REQUEST['temperature'] ?: "";

  // numeric(9,5)
  $clay_percentage = $_REQUEST['clay'] ?: "";

  // numeric(9,5)
  $sand_percentage = $_REQUEST['sand'] ?: "";

  // text
  $soilnotes = $_REQUEST['soilnotes'] ?: "";

  // text
  $notes = $_REQUEST['notes'] ?: "";

  // boolean = study conducted in a field which is 0 or greenhouse, pot, or growth chamber which is 1
  $greenhouse = (isset($_REQUEST['greenhouse'])) ? 1 : 0;

  // bigint, but needs to be a foreign key into the users table
  // not currently used
  $user_id = get_userid();


  // Insert new site
  $query = "INSERT INTO sites (sitename, geometry, greenhouse";
  if ($city != "") $query .= ", city";
  if ($state != "") $query .= ", state";
  if ($country != "") $query .= ", country";
  if ($time_zone != "") $query .= ", time_zone";
  if ($temperature != "") $query .= ", mat";
  if ($percipitation != "") $query .= ", map";
  if ($clay_percentage != "") $query .= ", clay_pct";
  if ($sand_percentage != "") $query .= ", sand_pct";
  if ($soilnotes != "") $query .= ", soilnotes";
  if ($notes != "") $query .= ", notes";
  if ($user_id != -1) $query .= ", user_id";
  $query .= ") VALUES (:sitename, ST_SetSRID(ST_MakePoint(:lon, :lat, :elevation), 4326), :greenhouse";
  if ($city != "") $query .= ", :city";
  if ($state != "") $query .= ", :state";
  if ($country != "") $query .= ", :country";
  if ($time_zone != "") $query .= ", :time_zone";
  if ($temperature != "") $query .= ", :temperature";
  if ($percipitation != "") $query .= ", :percipitation";
  if ($clay_percentage != "") $query .= ", :clay_percentage";
  if ($sand_percentage != "") $query .= ", :sand_percentage";
  if ($soilnotes != "") $query .= ", :soilnotes";
  if ($notes != "") $query .= ", :notes";
  if ($user_id != -1) $query .= ", user_id";
  $query .= ") RETURNING id, sitename";

  $stmt = $pdo->prepare($query);

  $stmt->bindParam(':sitename', $sitename, PDO::PARAM_STR);
  $stmt->bindParam(':lat', $lat, PDO::PARAM_STR); //decimal
  $stmt->bindParam(':lon', $lon, PDO::PARAM_STR); //decimal
  $stmt->bindParam(':elevation', $elevation, PDO::PARAM_STR);
  $stmt->bindParam(':greenhouse', $greenhouse, PDO::PARAM_BOOL);
  if ($city != "") $stmt->bindParam(':city', $city, PDO::PARAM_STR);
  if ($state != "") $stmt->bindParam(':state', $state, PDO::PARAM_STR);
  if ($country != "") $stmt->bindParam(':country', $country, PDO::PARAM_STR);
  if ($time_zone != "")$stmt->bindParam(':time_zone', $time_zone, PDO::PARAM_STR);
  if ($temperature != "") $stmt->bindParam(':temperature', $temperature, PDO::PARAM_STR);
  if ($percipitation != "") $stmt->bindParam(':percipitation', $percipitation, PDO::PARAM_INT);
  if ($clay_percentage != "") $stmt->bindParam(':clay_percentage', $clay_percentage, PDO::PARAM_STR);
  if ($sand_percentage != "") $stmt->bindParam(':sand_percentage', $sand_percentage, PDO::PARAM_STR);
  if ($soilnotes != "") $stmt->bindParam(':soilnotes', $soilnotes, PDO::PARAM_STR);
  if ($notes != "") $stmt->bindParam(':notes', $notes, PDO::PARAM_STR);
  if ($user_id != -1) $stmt->bindParam(':user_id', $user_id, PDO::PARAM_INT);

  if ($stmt->execute() === FALSE) {
    die('Can\'t insert new site query: ' . error_database());
    
  } else {
    $result = $stmt->fetch(PDO::FETCH_ASSOC);

    if ($sitegroupid != "-1") {
      $query = "INSERT INTO sitegroups_sites (sitegroup_id, site_id) VALUES (:sitegroup_id, :site_id)";
      $stmt = $pdo->prepare($query);
      $stmt->bindParam(':sitegroup_id', $sitegroupid, PDO::PARAM_INT);
      $stmt->bindParam(':site_id', $result['id'], PDO::PARAM_INT);
      if ($stmt->execute() === FALSE) {
        die('Can\'t insert new site into sitegroup: ' . error_database());
      }
    }

    echo "{$result['id']} {$result['sitename']} {$sitegroupid}";
  }

?>
