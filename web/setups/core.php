<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

/** This page contains the codes which are common in edit.php and add.php
  * As keeping single copy of code makes it easy to modify and maintain
  */

  // If file doesn't exist then create a new file
  if (file_exists ("../config.php") == false){
    header("Location: http://$host/setups/filesmissing.php",TRUE,302);
  }

  // key defines the attribute or the group of attributes which are needed to modify
  $key = $_GET['key'];
  $messagekey = $_GET['message'];

  $pattern = NULL;
  // set the pattern to match with the input

  switch ($key) {
    case 'all': $pattern = '/^\$/i'; break; // not working properly
    case 'database': $pattern = '/\$db_bety_*/i'; break;
    case 'fiadb': $pattern = '/\$db_fia_*/i'; break;
    case 'client_sceret': $pattern = '/\$client_sceret*/i'; break;
    default: $pattern = '/^\$'.$key.'/i';
  }

  // read content of file
  $file_contents = file('../config.php') or die('Cannot open file: Check whether file exist and it have correct permissions');

  // including the config.php so that the previous values can be used in the files inputs
  include '../config.php';

  // message for improving interaction with users
  $message=array(
    '1' => "It Seems there is some problem with getting $key, <br> Can you Verify the following credentials and try connecting again.",
    '2' => "It seems Config file doesn't exist, So I have created one but I need following details to properly establish the connection with other parts like database.</br><b>If you don't know any of these you can skip.</b>",
    '3' => "",
    '4' => "",
    );

  // always require user to be logged in.
  include '../common.php';
  open_database();
  if (!check_login()) {
    header( "Location: ../login.php");
    close_database();
    exit;
  }
  close_database();
?>
