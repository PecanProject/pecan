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

$client_sceret = $_POST['client_sceret'];
$server_auth_token = $_POST['server_auth_token'];
$fqdn = $_POST['fqdn'];

if (!(isset($client_sceret) && isset($server_auth_token))){
/**
 * token not set means client is a new one so add new data to the table and send
 * back the client tokens and sync id
 */
 //add code to create new client
 open_database();
 $stmt = $pdo->prepare("INSERT
   INTO machines (id, hostname, created_at, updated_at , sync_host_id, sync_url, sync_contact, sync_start, sync_end)
   VALUES (, :hostname, :created_at, :updated_at , :sync_host_id, :sync_url, :sync_contact, :sync_start, :sync_end );");
 if (!$stmt->execute(array(':hostname' => $fqdn,
                          ':created_at' => date("Y-m-d"),
                          ':updated_at' => date("Y-m-d"),
                          ':sync_host_id' => ,
                          ':sync_url' =>,
                          ':sync_contact' =>,
                          ':sync_start' => ,
                          ':sync_end' => ))) {
  echo json_encode(array('status' => 'ERROR',
                        'errormessage' => 'Invalid query : [' . error_database() . ']'  . $pdo->errorInfo()));
  die();
 }

}

$stmt = $pdo->prepare("SELECT * FROM machines WHERE hostname = ?");
if (!$stmt->execute(array($fqdn))) {
  echo json_encode(array('status' => 'ERROR',
                        'errormessage' => 'Invalid query : [' . error_database() . ']'  . $pdo->errorInfo()));
  die();
}
$row = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

// checking for existance and other things

$wantid = 1;      // Generate the wantid

echo json_encode(array('wantid' => $wantid)); // return the data

?>
