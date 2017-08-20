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

open_database();
if (!check_login()) {
  close_database();
  header( "Location: index.php");
  exit;
}
if (get_page_acccess_level() > $min_upload_level) {
  header( "Location: index.php");
  close_database();
  exit;
}
