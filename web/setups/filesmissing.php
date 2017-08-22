<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// Page to display if the config.php doesn't exist

$host  = $_SERVER['HTTP_HOST'];

include 'page.template.php';
?>
<div class="container-fluid">
<h2> It seems the config.php doesn't exist </h2>
<p>Please follow the following instructions to create a new config.php</p>
<ul class="list-group">
  <li class="list-group-item">Open terminal</li>
<?php
if ((preg_match("/loalhost*/",$host) || preg_match("/127.0.0.1*/",$host))) {
# running on localhost system
  $temp = preg_split('/:/',$host);
?>
  <li class="list-group-item">Type <pre>ssh <?php echo "carya@".$temp[0];?></pre> to establish the connection with remote host
    <br>If asked for password use the default password : illinois</li>
<?php
}
?>
  <li class="list-group-item">change the current working directory to pecan web directory <pre> cd ~/pecan/web </pre></li>
  <li class="list-group-item">There's a config.example.php create a copy of it and rename it as config.php <pre> cp config.example.php config.php </pre></li>
</ul>
</div>
<?php
include 'pagefooter.template.php';
?>
