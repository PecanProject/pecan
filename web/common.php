<?php

require("config.php");

# Single shared connection
$pdo=null;

# make sure we do a session start
session_start();

function get_footer() {
  return "The <a href=\"http://pecanproject.org\">PEcAn project</a> is supported by the National Science Foundation
    (ABI #1062547, ABI #1458021, DIBBS #1261582, ARC #1023477, EF #1318164, EF #1241894, EF #1241891), NASA
    Terrestrial Ecosystems, Department of Energy (ARPA-E #DE-AR0000594 and #DE-AR0000598), 
    Department of Defense, the Arizona Experiment Station, the Energy Biosciences Institute, 
    and an Amazon AWS in Education Grant.
    <span style=\"float:right\">PEcAn Version 1.8.0.9000</span>";
}

function whoami() {
  if (check_login()) {
    echo "<p></p>";
    echo "Logged in as " . get_user_name();
    echo "<a style=\"float: right;\" href=\"index.php?logout\" id=\"logout\">logout</a>";
  } else {
    echo "<p></p>";
    echo "Not Logged in.";
    echo "<a style=\"float: right;\" href=\"login.php\">login</a>";
  }
}

function left_footer() {
  if (check_login()) {
    echo "<p></p>";
    echo "Logged in as " . get_user_name();
    echo "<a style=\"float: right;\" href=\"index.php?logout\" id=\"logout\">logout</a>";
  } else {
    echo "<p></p>";
    echo "Not Logged in.";
    echo "<a style=\"float: right;\" href=\"login.php\">login</a>";
  }

?>
<p></p>
  <a href="https://pecanproject.github.io/pecan-documentation/master" target="_blank">Documentation</a>
  <br>
  <a href="https://join.slack.com/t/pecanproject/shared_invite/enQtMzkyODUyMjQyNTgzLWEzOTM1ZjhmYWUxNzYwYzkxMWVlODAyZWQwYjliYzA0MDA0MjE4YmMyOTFhMjYyMjYzN2FjODE4N2Y4YWFhZmQ" target="_blank">Chat Room</a>
  <br>
  <a href="http://pecanproject.github.io/Report_an_issue.html" target="_blank">Bug Report</a>
<?php
}

function passvars($ignore) {
  foreach($_REQUEST as $key => $value) {
    if (!array_key_exists($key, $ignore)) {
      if (is_array($value)) {
        foreach($value as $v) {
          echo "<input name=\"{$key}[]\" id=\"{$key}[]\" type=\"hidden\" value=\"{$v}\"/>";
        }
      } else {
        echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$value}\"/>";
      }
    }
  }
}
# ----------------------------------------------------------------------
# CONVERT STRING TO XML
# ----------------------------------------------------------------------
function toXML($string) {
  //return htmlspecialchars($string, ENT_XML1);
  return strtr($string, array('"'=> "&quot;", "&" => "&amp;", "'"=> "&apos;", "<" => "&lt;", ">"=> "&gt;"));
}

# ----------------------------------------------------------------------
# DATABASE FUNCTIONS
# ----------------------------------------------------------------------
function open_database() {
  global $db_bety_hostname;
  global $db_bety_port;
  global $db_bety_username;
  global $db_bety_password;
  global $db_bety_database;
  global $db_bety_type;
  global $pdo;

  try {
    $pdo = new PDO("{$db_bety_type}:host={$db_bety_hostname};dbname={$db_bety_database};port={$db_bety_port}", $db_bety_username, $db_bety_password);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
  } catch (PDOException $e) {
    // handler to input database configurations manually
    $host  = $_SERVER['HTTP_HOST'];
    echo "Something wrong :(</br>Connection failed: " . $e->getMessage();
    die();
  }
//  $pdo = new PDO("{$db_bety_type}:host={$db_bety_hostname};dbname={$db_bety_database}", $db_bety_username, $db_bety_password);
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

# Create a new RabbitMQ connection
# Global variables are set in config.php
function make_rabbitmq_connection($rabbitmq_uri) {
  $rabbitmq = parse_url($rabbitmq_uri);
  $connection = new AMQPConnection();
  if (!empty($rabbitmq['host'])) {
    $connection->setHost($rabbitmq['host']);
  }
  if (!empty($rabbitmq['port'])) {
    $connection->setPort($rabbitmq['port']);
  }
  if (!empty($rabbitmq['path'])) {
    $connection->setVhost(urldecode(ltrim($rabbitmq['path'], '/')));
  }
  if (!empty($rabbitmq['user'])) {
    $connection->setLogin($rabbitmq['user']);
  }
  if (!empty($rabbitmq['pass'])) {
    $connection->setPassword($rabbitmq['pass']);
  }
  $connection->connect();
  return $connection;
}

# Post $message (string) to RabbitMQ queue $rabbitmq_queue (string)
function send_rabbitmq_message($message, $rabbitmq_uri, $rabbitmq_queue) {
  $connection = make_rabbitmq_connection($rabbitmq_uri);
  $channel = new AMQPChannel($connection);
  $exchange = new AMQPExchange($channel);

  $exchange->publish($message, $rabbitmq_queue);
  $connection->disconnect();
}

?>
