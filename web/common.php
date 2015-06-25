<?php

require("config.php");

# Single shared connection
$pdo=null;

# make sure we do a session start
session_start();

function get_footer() {
  return "The <a href=\"http://pecanproject.org\">PEcAn project</a> is supported by the National Science Foundation
    (ABI #1062547, ARC #1023477) and the <a href=\"http://www.energybiosciencesinstitute.org/\">Energy
    Biosciences Institute</a>. <span style=\"float:right\">PEcAn Version 1.4.2</span>";
}

# ----------------------------------------------------------------------
# DATABASE FUNCTIONS
# ----------------------------------------------------------------------
function open_database() {
	global $db_bety_hostname;
	global $db_bety_username;
	global $db_bety_password;
	global $db_bety_database;
	global $db_bety_type;
	global $pdo;

	$pdo = new PDO("${db_bety_type}:host=${db_bety_hostname};dbname=${db_bety_database}", $db_bety_username, $db_bety_password);
}

function close_database() {
	global $pdo;
	$pdo = null;
}

function error_database() {
	global $pdo;
	$tmp = $pdo->errorInfo();
	return $tmp[2];
}

# ----------------------------------------------------------------------
# USER FUNCTIONS
# ----------------------------------------------------------------------
function login($username, $password) {
	global $pdo;

	if (isset($_SESSION['userid']) && ($username == $_SESSION['userid'])) {
		return TRUE;
	}

	if ($pdo == null) {
		open_database();
	}

	$stmt = $pdo->prepare("SELECT * FROM users WHERE login=?");
	if (!$stmt->execute(array($username))) {
		die('Invalid query : [' . error_database() . ']'  . $pdo->errorInfo());
	}

	$row = $stmt->fetch(PDO::FETCH_ASSOC);
	$stmt->closeCursor();
	if (!isset($row['salt'])) {
		return FALSE;
	}

	$digest = encrypt_password($password, $row['salt']);

	if ($digest == $row['crypted_password']) {
		$_SESSION['userid']=$row['id'];
		$_SESSION['username']=$row['name'];
		$_SESSION['useraccess']=$row['access_level'];
		$_SESSION['userpageaccess']=$row['page_access_level'];
		return TRUE;
	} else {
		return FALSE;
	}
}

function encrypt_password($password, $salt) {
	global $REST_AUTH_SITE_KEY;
	global $REST_AUTH_DIGEST_STRETCHES;

	$digest=$REST_AUTH_SITE_KEY;
	for($i=0; $i<$REST_AUTH_DIGEST_STRETCHES; $i++) {
	  $digest=sha1($digest . "--" . $salt . "--" . $password . "--" . $REST_AUTH_SITE_KEY);
	}
	return $digest;	
}

function logout() {
	unset($_SESSION['userid']);
	unset($_SESSION['username']);
	unset($_SESSION['useraccess']);
	unset($_SESSION['userpageaccess']);
}

function get_userid() {
	if (isset($_SESSION['userid'])) {
		return $_SESSION['userid'];
	} else {
		return -1;
	}
}

function check_login() {
	return isset($_SESSION['userid']);
}

function get_user_name() {
	if (isset($_SESSION['username'])) {
		return $_SESSION['username'];
	} else {
		return FALSE;
	}
}

function get_acccess_level() {
	global $anonymous_level;
	if (isset($_SESSION['useraccess'])) {
		return $_SESSION['useraccess'];
	} else {
		return $anonymous_level;
	}
}

function get_page_acccess_level() {
	global $anonymous_page;
	if (isset($_SESSION['userpageaccess'])) {
		return $_SESSION['userpageaccess'];
	} else {
		return $anonymous_page;
	}
}

?>
