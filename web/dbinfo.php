<?php
$db_hostname="localhost";
$db_username="bety";
$db_password="bety";
$db_database="bety";

function open_database() {
	global $db_hostname;
	global $db_username;
	global $db_password;
	global $db_database;
	
	// Opens a connection to a MySQL server
	$connection=mysql_connect ($db_hostname, $db_username, $db_password);
	if (!$connection) {
		die('Not connected : ' . mysql_error());
	}
	
	// Set the active MySQL database
	$db_selected = mysql_select_db($db_database, $connection);
	if (!$db_selected) {
		die ('Can\'t use db : ' . mysql_error());
	}
	
	return $connection;
}

function close_database($connection) {
	mysql_close($connection);
}
?>
