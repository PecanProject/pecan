<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// This page is the interface to edit any configuration metioned in config.php

include 'core.php';
include 'page.template.php';
?>
  <form id="formnext" method="POST" action="<?php echo"/setups/add.php?key=$key";?>">
    <h1><?php echo "$key "; ?>Configuration details</h1>
<?php
foreach ($file_contents as $line) {
  //echo "$line<br>";
  if(preg_match($pattern,$line)){
    //spliting variable and values so can used variable as the input field names
    $temp = preg_split('/=/',$line);
    $inputname = preg_split('/\$/',$temp[0]);
    //var_dump($inputname);
    // HTML input code for field input;
    ?>
    <label><?php echo $inputname[1]; ?> : </label>
    <input name="<?php echo $inputname[1]; ?>" id="username" style="align: left" type="text" value="<?php echo ${$inputname[1]};?>">
    <div class="spacer"></div>
    <?php
    //var_dump($temp);
    //echo "match found <br>";
  }
}
?>
    <button type="submit" id="next">Submit</button>
    <div class="spacer"></div>
  </form>
<?php
include 'pagefooter.template.php';
 ?>
