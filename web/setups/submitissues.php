<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// bug reporting page page

$title = $_POST['title'];
$description = $_POST['description'];
$username = $_POST['username'];
$password = $_POST['password'];

//flags
$isCurlFailed = FALSE;

include 'page.template.php';
echo "<h1> Report the bug</h1>";

if (isset($title) && !empty($title) &&
    isset($description) && !empty($description) &&
    isset($username) && !empty($username) &&
    isset($password) && !empty($password)) {

      $service_url = "https://api.github.com/repos/PecanProject/pecan/issues";

      $user_agent = "Pecan-application";
      $curl = curl_init($service_url);
      
      $curl_post_data = array ('title' => $title,
                               'body' => $description,
                               'lables' => 'Bug' );

      $data_string = json_encode($curl_post_data);

      // setting curl to do a POST request
      curl_setopt($curl, CURLOPT_USERPWD, $username.":".$password);   // authentication
      curl_setopt($curl, CURLOPT_USERAGENT, $user_agent);
      curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
      curl_setopt($curl, CURLOPT_POST, true);
      curl_setopt($curl, CURLOPT_POSTFIELDS, $data_string);
      curl_setopt($curl, CURLOPT_HTTPHEADER, array(
        'Content-Type: application/json',
        'Content-Length: ' . strlen($data_string))
      );

      // execute the curl request
      $curl_response = curl_exec($curl);

      $decode =  json_decode($curl_response);

      //var_dump($decode);
      if (curl_getinfo ($curl, CURLINFO_HTTP_CODE) == 201)
      {
// Generate tha page to inform the user
?>
        <div class="alert alert-success" role="alert">
          Bug Report Sucessfully submitted
        </div>
        <div class="alert alert-info" role="alert">
          You can refer it by visiting the following link
          <a href="<?php echo $decode->html_url; ?>"> <?php echo $decode->html_url; ?></a>
        </div>

        <div class="alert alert-info" role="alert">
          You can submit another one
        </div>

<?php
      }else {
?>
        <div class="alert alert-danger" role="alert">Failed to submit the Bug Report</div>
        <div class="alert alert-warning" role="alert">Please check your github credentials</div>
<?php
      }

}
// form for the user input
?>
<form class="form-horizontal" role="form" id="formnext" method="POST" action="submitissues.php" enctype="multipart/form-data">
 <h3>All fields are important</h3>
  <div class="form-group">
      <label for="connection" class="col-md-4 control-label">Title : </label>
      <div class="col-md-6">
          <input name="title" id="title" type="text" class="form-control transparent-input" placeholder="bug title">
      </div>
    <?php if (isset($title) && empty($title)) {?>
      <span class="help-block">
          <strong>Title Needed</strong>
      </span>
    <?php } ?>
  </div>
  <div class="form-group">
      <label for="connection" class="col-md-4 control-label">Description : </label>
      <div class="col-md-6">
          <textarea name="description" rows="13" id="description" type="textbox" class="form-control transparent-input" >
<!--- Provide a general summary of the issue in the Title above -->
## Description
<!--- What change or feature do you propose? -->

## Context
<!--- Why is this change important to you? How would you use it? -->
<!--- How can it benefit other users and communities? -->
<!--- Is there existing software with a similar featuer? -->

## Possible Implementation
<!--- Not obligatory, but suggest an idea for implementing addition or change -->
        </textarea>
      </div>
      <?php if (isset($description) && empty($description)) {?>
        <span class="help-block">
            <strong>Description Needed</strong>
        </span>
      <?php } ?>
  </div>
  <div class="form-group">
    <label for="connection" class="col-md-7 control-label">Need your Github credential to post the bug</label>
    <div class="col-md-3">

    </div>
  </div>
  <div class="form-group">
      <label for="connection" class="col-md-4 control-label">Github username / email :</label>
      <div class="col-md-3">
          <input name="username" id="username" type="text" class="form-control transparent-input" placeholder="username / email ">
      </div>
      If u don't have Github account you can create one <a href="https://github.com/join">here</a>
      <?php if (isset($username) && empty($username)) {?>
        <span class="help-block">
            <strong>Github Username / Email is must</strong>
        </span>
      <?php } ?>
  </div>
  <div class="form-group">
      <label for="connection" class="col-md-4 control-label">Password: </label>
      <div class="col-md-3">
          <input name="password" id="password" type="password" class="form-control transparent-input" placeholder=" ">
      </div>
      <?php if (isset($password) && empty($password)) {?>
        <span class="help-block">
            <strong>Github password is must</strong>
        </span>
      <?php } ?>

  </div>
  <div class="form-group">
      <div class="col-md-6 col-md-offset-4">
          <button type="submit" class="btn btn-primary">
              <i class="fa fa-btn fa-send"></i> Submit Bug
          </button>
      </div>
  </div>
  <!--<button type="submit" id="next">Submit</button>-->
  <div class="spacer"></div>
</form>

<?php

footer:
include 'pagefooter.template.php';

?>
