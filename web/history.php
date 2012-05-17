<?php

// database parameters
require("dbinfo.php");

// Opens a connection to a MySQL server
$connection=mysql_connect ($hostname, $username, $password);
if (!$connection) {
	die('Not connected : ' . mysql_error());
}

// Set the active MySQL database
$db_selected = mysql_select_db($database, $connection);
if (!$db_selected) {
	die ('Can\'t use db : ' . mysql_error());
}

// get run information
$query = "SELECT * FROM runs";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}

echo "<h1>Runs</h1>";
echo "<table border=1>";
while ($row = @mysql_fetch_assoc($result)) {
  echo "<tr>";
  echo "<td><a href=\"running.php?runid={$row['id']}\">{$row['id']}</a></td>";
  echo "<td>{$row['site_id']}</td>";
  echo "<td>{$row['start_time']}</td>";
  echo "<td>{$row['finish_time']}</td>";
  echo "<td>{$row['outdir']}</td>";
  echo "<td>{$row['started_at']}</td>";
  echo "<td>{$row['finished_at']}</td>";
  echo "</tr>";
}
echo "</table>";
?>

