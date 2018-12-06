<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// page to display instructions to change password

$host  = $_SERVER['HTTP_HOST'];

include 'page.template.php';
?>
<div class="container-fluid">
<h2> Please follow these instructions to change the VM user password </h2>
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
  <li class="list-group-item">Type <pre>passwd</pre> It will ask for the current password,<br>Input Users the current password (default password illinois)</li>
  <li class="list-group-item">It will ask for the new password,<br>Input Users the current password</li>
  <li class="list-group-item">It will ask for the new password again (conform password),<br>Input Users the current password</li>
  <li class="list-group-item"><b>Note : password wont be displayed not even stars or dot</b></li>
</ul>
</div>
<?php
include 'pagefooter.template.php';
?>
