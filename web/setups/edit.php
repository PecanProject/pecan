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
  <form class="form-horizontal" role="form" id="formnext" method="POST" action="<?php echo"add.php?key=$key";?>" enctype="multipart/form-data">
    <h1><?php echo ucfirst($key); ?> Configuration details</h1>
<?php
if (isset($messagekey))
{
  echo "<p>$message[$messagekey]</p>";
}
foreach ($file_contents as $line) {
  if(preg_match($pattern,$line)){
    //spliting variable and values so can used variable as the input field names
    $temp = preg_split('/=/',$line);
    $inputname = preg_split('/\$/',$temp[0]);
    // HTML input code for field input;
    ?>
    <div class="form-group">
        <label for="connection" class="col-md-4 control-label"><?php echo ucfirst($inputname[1]); ?> : </label>
        <div class="col-md-6">
            <input name="<?php echo $inputname[1]; ?>" id="username" type="text" class="form-control transparent-input" value="<?php echo ${$inputname[1]};?>">
        </div>
    </div>
    <?php
  }
}
?>
    <div class="form-group">
        <div class="col-md-6 col-md-offset-4">
            <button type="submit" class="btn btn-primary">
                <i class="fa fa-btn fa-save"></i> Save
            </button>
        </div>
    </div>
    <!--<button type="submit" id="next">Submit</button>-->
    <div class="spacer"></div>
  </form>
<?php
include 'pagefooter.template.php';
?>
