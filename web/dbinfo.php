<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

require("system.php");

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
