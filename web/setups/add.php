<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// This page adds the configuration edited in edit.php to in config.php
include 'core.php';

// open a new temprory file to write data to it
$tempfile = tmpfile();

foreach ($file_contents as $line) {
  if(preg_match($pattern,$line)){
    //spliting variable and values so can used variable as the input field names
    $temp = preg_split('/=/',$line);
    $inputname = preg_split('/\$/',$temp[0]);

    // get the new value from the post request
    $newvalue = $_POST[$inputname[1]];

    fwrite($tempfile, $temp[0].'="'.$newvalue.'";'."\n");
  }
  else {
  // if no change in the line write as it is
  fwrite($tempfile, $line);
  }
}

$configfile = fopen("../config.php", "w+");

rewind($tempfile);

while (($buffer=fgets($tempfile))!== false) {
  fwrite($configfile,$buffer);
}

fclose($tempfile); // remove tempfile

include 'page.template.php';
?>
  <h1><?php echo ucfirst($key); ?> Configuration details</h1>
  <div class="alert alert-success" role="alert"> Configuration Sucessfully updated </div>
<?php
include 'pagefooter.template.php';
?>
