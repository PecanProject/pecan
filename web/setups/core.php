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
    if (!copy ('../config.example.php', '../config.php')){
      echo "error: permissions denined";
      die();
    }
  }

  // key defines the attribute or the group of attributes which are needed to modify
  $key = $_GET['key'];

  $pattern = NULL;
  // set the pattern to match with the input

  switch ($key) {
    case 'all': $pattern = '/^\$/i'; break; // not working properly
    case 'browndog': $pattern = '/\$browndog*/i'; break;
    case 'database': $pattern = '/\$db_bety_*/i'; break;
    case 'fiadb': $pattern = '/\$db_fia_*/i'; break;
    default: $pattern = '/^\$'.$key.'/i';
  }

  // read content of file
  //$file = fopen('../config.php', "c+") or die('Cannot open file: Check whether file exist and it have correct permissions');

  $file_contents = file('../config.php') or die('Cannot open file: Check whether file exist and it have correct permissions');

  //var_dump($file_contents);
  //var_dump($pattern);

  // including the config.php so that the previous values can be used in the files inputs
  include '../config.php';

?>
