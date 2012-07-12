<?php

// database parameters
require("dbinfo.php");
$connection = open_database();

// get run information
$query = "SELECT * FROM workflows";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}

echo "<h1>Executions</h1>";
echo "<table border=1>";
echo "<tr>";
echo "<th>ID</th>";
echo "<th>site id</th>";
echo "<th>model id</th>";
echo "<th>model type</th>";
echo "<th>start date</th>";
echo "<th>end date</th>";
echo "<th>started</th>";
echo "<th>finished</th>";
echo "</tr>";
while ($row = @mysql_fetch_assoc($result)) {
  echo "<tr>";
  echo "<td><a href=\"running.php?workflowid={$row['id']}\">{$row['id']}</a></td>";
  echo "<td>{$row['site_id']}</td>";
  echo "<td>{$row['model_id']}</td>";
  echo "<td>{$row['model_type']}</td>";
  echo "<td>{$row['start_date']}</td>";
  echo "<td>{$row['end_date']}</td>";
  echo "<td>{$row['started_at']}</td>";
  echo "<td>{$row['finished_at']}</td>";
  echo "</tr>";
}
echo "</table>";

close_database($connection);
?>

