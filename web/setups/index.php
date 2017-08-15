<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// this is the welcome page for the configuration setup
include 'core.php';
include 'page.template.php';
?>
  <h1>Introduction</h1>
  <p>Welcome to the Configuration page.</p>
  <p>Side panel consist of all the available configuarations.</p>
<?php
if (isset($messagekey))
{
  echo "<p>$message[$messagekey]</p>";
?>
  <a href="edit.php?key=database" class="btn btn-primary">Next</a>
<?php
}

include 'pagefooter.template.php';
?>
