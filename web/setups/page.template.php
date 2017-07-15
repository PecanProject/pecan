<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// This page is designed to act as the template page for all the configurations setups
?>
<!DOCTYPE html>
<html lang="en">
<head>
<title>PEcAn Configurations</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="../sites.css" />
</head>
<body>
<div id="wrap">
    <div id="stylized">
        <form id="formprev" method="POST" action="history.php">
        </form>
        <form id="formnext" method="POST" action="02-modelsite.php">
            <h1>Introduction</h1>
            <p>On this page you will find the required fields to setup the particular configurations or edit it.</p>

            <div class="spacer"></div>
        </form>
<p>
  <h1>List of available configurations</h1>
  <a href="edit.php?key=database" >Database</a> <br>
  <a href="edit.php?key=browndog" >Browndog</a> <br>
  <a href="edit.php?key=fiadb" >FIA Database</a> <br>
  <a href="edit.php?key=googleMapKey" >Google MapKey</a> <br>
</p>
<p>
  <a href="https://pecan.gitbooks.io/pecan-documentation/content/" target="_blank">Documentation</a>
<br>
  <a href="https://gitter.im/PecanProject/pecan" target="_blank">Chat Room</a>
<br>
  <a href="https://github.com/PecanProject/pecan/issues/new" target="_blank">Bug Report</a>
</p>
    </div>
    <div id="output">
