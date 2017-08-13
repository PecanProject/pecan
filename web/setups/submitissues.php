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




include 'page.template.php';
?>

<form class="form-horizontal" role="form" id="formnext" method="POST" action="submitissues.php" enctype="multipart/form-data">
  <h1> Report the bug</h1>

  <div class="form-group">
      <label for="connection" class="col-md-4 control-label">Title : </label>
      <div class="col-md-6">
          <input name="Title" id="Title" type="text" class="form-control transparent-input" placeholder="bug title">
      </div>
  </div>
  <div class="form-group">
      <label for="connection" class="col-md-4 control-label">Description : </label>
      <div class="col-md-6">
          <textarea name="description" rows="5" id="description" type="textbox" class="form-control transparent-input" >
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
      If u don't have Github account you can create one <a href="https://github.com/join">here</a>.
  </div>

  <div class="form-group">
      <label for="connection" class="col-md-4 control-label">Password: </label>
      <div class="col-md-3">
          <input name="password" id="password" type="password" class="form-control transparent-input" placeholder=" ">
      </div>
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

include 'pagefooter.template.php';

?>
