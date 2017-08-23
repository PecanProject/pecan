<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// cronjob

if (file_exists ("syncflag.txt") == true){
  include 'sync.php';
}

?>
