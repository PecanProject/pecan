<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// Server side scripts to handle the client resquests

include_once('../common.php');

header("Content-Type: application/json");
//header("HTTP/1.0 404 Not Found");

$client_sceret = $_POST['client_sceret'];
$server_auth_token = $_POST['server_auth_token'];
$fqdn = $_POST['fqdn'];

open_database();

$stmt = $pdo->prepare("SELECT * FROM machines WHERE hostname = :hostname;",array(PDO::ATTR_CURSOR => PDO::CURSOR_FWDONLY));

if (!$stmt->execute(array(':hostname' => $fqdn))) {
  echo json_encode(array('status' => 'ERROR',
                        'errormessage' => 'Invalid query : [' . error_database() . ']'  . $pdo->errorInfo()));
  die();
}

$row = $stmt->fetch(PDO::FETCH_ASSOC);

//var_dump($row);

if ($row == false) {
  // means no data exist in database

  // finding the max sync_id
  $stmt = $pdo->prepare("SELECT max(sync_host_id) FROM machines;",array(PDO::ATTR_CURSOR => PDO::CURSOR_FWDONLY));
  if (!$stmt->execute()) {
    echo json_encode(array('status' => 'ERROR',
                          'errormessage' => 'Invalid query : [' . error_database() . ']'  . $pdo->errorInfo()));
    die();
  }
  $row = $stmt->fetch(PDO::FETCH_ASSOC);

  $host_id = $row+1;

  // if the host_id == 99 then changing it to 100 as 99 is reserved for the VM
  if ($host_id == 99)
    $host_id++;

  // finding max id
  $stmt = $pdo->prepare("SELECT max(id) FROM machines;",array(PDO::ATTR_CURSOR => PDO::CURSOR_FWDONLY));
  if (!$stmt->execute()) {
    echo json_encode(array('status' => 'ERROR',
                          'errormessage' => 'Invalid query : [' . error_database() . ']'  . $pdo->errorInfo()));
    die();
  }
  $row = $stmt->fetch(PDO::FETCH_ASSOC);

  $id = $row+1;

  $date = date("Y-m-d H:i:s");
  $stmt = $pdo->prepare("INSERT
    INTO machines (id, hostname, sync_host_id, sync_url, sync_contact, sync_start, sync_end)
    VALUES (:id, :hostname, :sync_host_id, :sync_url, :sync_contact, :sync_start, :sync_end );");
  $stmt->bindValue(':id', $id, PDO::PARAM_INT);
  $stmt->bindValue(':hostname', $fqdn, PDO::PARAM_STR);
  $stmt->bindValue(':sync_host_id', $host_id, PDO::PARAM_INT);
  $stmt->bindValue(':sync_url', ' ', PDO::PARAM_STR);
  $stmt->bindValue(':sync_contact', ' ', PDO::PARAM_STR);
  $stmt->bindValue(':sync_start', $host_id * (10 ^ 9), PDO::PARAM_INT);
  $stmt->bindValue(':sync_end', $host_id*(10^9)+(10^9)-1, PDO::PARAM_INT);
  if (!$stmt->execute()) {
   echo json_encode(array('status' => 'ERROR',
                         'errormessage' => 'Invalid query : [' . error_database() . ']'  . $pdo->errorInfo()));
   die();
  }

  $client_sceret = md5($fqdn.$date);

}
else {
  // means row details exist so verify the  client authentication keys
  //var_dump($row);
  if ($client_sceret != md5($row['hostname'].$row['created_at'])) {
    echo json_encode(array('status' => 'ERROR',
                          'errormessage' => 'Invalid client_sceret'));
    die();
  }

}

$stmt->closeCursor();

// checking for existance and other things

$wantid = $row->sync_host_id;      // Generate the wantid

echo json_encode(array('status' => 'OK',
                       'wantid' => $wantid,
                       'client_sceret' => $client_sceret)); // return the data

?>
