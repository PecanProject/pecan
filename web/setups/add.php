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
$file = fopen('../config.php.temp', "w+") or die('Cannot open file: Check whether file exist and it have correct permissions');

//fwrite($file, '<?php \n');

foreach ($file_contents as $line) {
  //echo "$line<br>";
  if(preg_match($pattern,$line)){
    //spliting variable and values so can used variable as the input field names
    $temp = preg_split('/=/',$line);
    $inputname = preg_split('/\$/',$temp[0]);

    // get the new value from the post request
    $newvalue = $_POST[$inputname[1]];

    //$newline = preg_replace('/'.$temp[0].'/',$temp[0].'="'.$newvalue.'";',$line);
    //echo $temp[0].'="'.$newvalue.'";';
    //var_dump($newvalue);
    fwrite($file, $temp[0].'="'.$newvalue.'";'."\n");
    //var_dump($inputname);
    //var_dump($temp);
    //echo "match found <br>";
  }
  else {
  // if no change in the line write as it is
  fwrite($file, $line);
  }
}
fclose($file);

// copy the temprory file to config.php and remove it
rename('../config.php.temp', '../config.php');
unlink('../config.php.temp');

include 'page.template.php';
?>
  <h1><?php echo "$key "; ?>Configuration details</h1>
  <p>Configuration Sucessfully updated</p>
<?php
include 'pagefooter.template.php';
?>
