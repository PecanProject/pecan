<?php

require("config.php");

# Single shared connection
$pdo=null;

# make sure we do a session start
session_start();

# ----------------------------------------------------------------------
# DATABASE FUNCTIONS
# ----------------------------------------------------------------------
function open_database() {
	global $db_hostname;
	global $db_username;
	global $db_password;
	global $db_database;
	global $db_type;
	global $pdo;

	$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", $db_username, $db_password);
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
