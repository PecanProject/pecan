<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// Check login
require("common.php");
if (isset($_REQUEST['logout'])) {
	logout();
}

if ($authentication) {
  header( "Location: login.php");
} else {
  header( "Location: 01-introduction.php");
}
