<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// change password script location
$shellscript = "expect chpasswd.exp";

$username = $_POST['username'];
$oldpasswd = $_POST['oldpasswd'];
$newpasswd = $_POST['newpasswd'];
$newpasswdre = $_POST['newpasswdre'];

# used as a flag to determine the action to be taken according to the request
$callchpasswd = false;

if (isset($username) && isset($oldpasswd) && isset($newpasswd) && isset($newpasswdre)) {
  if (!empty($username) && !empty($oldpasswd) && !empty($newpasswd) && !empty($newpasswdre)) {
    if ($newpasswd == $newpasswdre) {
      $callchpasswd = true;
    }
  }
}

// include the page template
include 'page.template.php';

if ($callchpasswd) {
  // generate the command to execute
  $cmd="$shellscript " . $username. " " . $oldpasswd . " " . $newpasswd;
  // execute the command
  exec($cmd,$output,$status);

  if ($status == 0) {
    echo "password chaged sucessfully";
  } else {
    echo "ERROR : error changing password";
    echo "<br>";
    print_r($output);
  }
}else {
?>
<div class="container">
    <div class="row">
      <h1>Password Change</h1>
      <form class="form-horizontal" role="form" method="POST" action="chpasswd.php" enctype="multipart/form-data">
        <div class="form-group">
          <label for="username" class="col-md-4 control-label">Username :</label>
          <div class="col-md-6">
            <input id="username" type="text" class="form-control transparent-input" name="username" value="">
          </div>
        </div>
        <div class="form-group">
          <label for="password" class="col-md-4 control-label">old Password:</label>
          <div class="col-md-6">
            <input id="password" type="password" class="form-control transparent-input" name="oldpasswd" value="">
          </div>
        </div>
        <div class="form-group">
          <label for="password" class="col-md-4 control-label">New Password:</label>
          <div class="col-md-6">
            <input id="password" type="password" class="form-control transparent-input" name="newpasswd" value="">
          </div>
        </div>
        <div class="form-group">
          <label for="password" class="col-md-4 control-label">New Password (again):</label>
          <div class="col-md-6">
            <input id="password" type="password" class="form-control transparent-input" name="newpasswdre" value="">
        </div>
        </div>
        <div class="form-group">
         <div class="col-md-6 col-md-offset-4">
           <button type="submit" class="btn btn-primary">
             <i class="fa fa-btn fa-user"></i> Update
           </button>
         </div>
       </div>
     </form>
    </div>
</div>

<?php
}

include 'pagefooter.template.php';
?>
