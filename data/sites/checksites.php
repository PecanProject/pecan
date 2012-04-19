#!/usr/bin/php

<?php
$db_user = "ebi_user";
$db_pass = "mScGKxhPhdq";
$db_db   = "ebi_analysis";
$db_host = "localhost";

$accuracy = 0.5;

// Opens a connection to a MySQL server
$connection = mysql_connect($db_host, $db_user, $db_pass);
if (!$connection) {
  die("Not connected : " . mysql_error());
}

// Set the active MySQL database
$db_selected = mysql_select_db($db_db, $connection);
if (!$db_selected) {
  die("Can\'t use db : " . mysql_error());
}

// Select all the rows in the site table
$query = "select id, city, state, country, lat, lon from sites";
$result = mysql_query($query);
if (!$result) {
  die("Invalid query: " . mysql_error());
}

// Initialize delay in geocode speed
$delay = 0;
$base_url = "http://maps.google.com/maps/geo?output=xml&q=";

// Iterate through the rows, geocoding each address
print "STATUS, ID, GOOGLE_LAT, GOOGLE_LON, DB_LAT, DB_LON, CITY, STATE, COUNTRY\n";
while ($row = @mysql_fetch_assoc($result)) {
  $address = $row["city"] . ", " . $row["state"] . ", " . $row["country"];
  $id = $row["id"];

  if (($row["country"] == "") && ($row["city"] == "")) {
    print "ADDRESS, $id, , , " . $row["lat"] . ", " . $row["lon"] . ", $address\n";
    continue;
  }
  if ($row["country"] == "") {
    print "COUNTRY, $id, , , " . $row["lat"] . ", " . $row["lon"] . ", $address\n";
    continue;
  }
  if ($row["city"] == "") {
    print "CITY, $id, , , " . $row["lat"] . ", " . $row["lon"] . ", $address\n";
    continue;
  }

  $geocode_pending = true;
  $request_url = $base_url . urlencode($address);
  while ($geocode_pending) {
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, $request_url);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    // get the result of http query
    $output = curl_exec($ch);
    curl_close($ch);
    // feed the curl output to simplexml_load_string
    $xml = simplexml_load_string($output) or die("XML string not loading");
    #$xml = simplexml_load_file($request_url) or die("url not loading");

    $status = $xml->Response->Status->code;
    if (strcmp($status, "200") == 0) {
      // Successful geocode
      $geocode_pending = false;
      $coordinates = $xml->Response->Placemark->Point->coordinates;
      $coordinatesSplit = preg_split("/,/", $coordinates);
      // Format: Longitude, Latitude, Altitude
      $lat = $coordinatesSplit[1];
      $lng = $coordinatesSplit[0];

      if (abs($lat - $row["lat"]) > $accuracy || abs($lng - $row["lon"]) > $accuracy) {
        print "ERROR, $id, $lat, $lng, " . $row["lat"] . ", " . $row["lon"] . ", $address\n";
      } else {
        print "OK, $id, $lat, $lng, " . $row["lat"] . ", " . $row["lon"] . ", $address\n";
      }
    } else if (strcmp($status, "620") == 0) {
      // sent geocodes too fast
      $delay += 100000;
    } else {
      // failure to geocode
      $geocode_pending = false;
      print "UNKNOWN, $id, , , " . $row["lat"] . ", " . $row["lon"] . ", $address\n";
    }
    usleep($delay);
  }
}
?>
