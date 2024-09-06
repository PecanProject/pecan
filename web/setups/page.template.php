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
<link rel="shortcut icon" type="image/x-icon" href="../favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="../sites.css" />
<!-- Fonts -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.5.0/css/font-awesome.min.css" integrity="sha384-XdYbMnZ/QjLh6iI4ogqCTaIjrFk87ip+ekIjefZch0Y+PvJ8CDYtEs1ipDmPorQ+" crossorigin="anonymous">
<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:100,300,400,700">

<!-- Styles -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.6/css/bootstrap.min.css" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous">
<link href="https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css" rel="stylesheet">
<style>
    body {
        font-family: 'Lato';
    }

    .fa-btn {
        margin-right: 6px;
    }
</style>
</head>
<body>
<div id="wrap">
    <div id="stylized">
      <h1>Introduction</h1>
      <p>This is the Admin Pages.
        <h1>List of available configurations</h1>
        <a href="edit.php?key=database" >Database</a> <br>
        <a href="edit.php?key=fiadb" >FIA Database</a> <br>
        <a href="chpasswd.php" >Change Password</a> <br>
      </p>
        <div class="checkbox disabled">
          <h1>Automatic Sync</h1> <input type="checkbox" disabled data-toggle="toggle">
        </div>
      <p></p>
      <p>
        <a href="https://pecan.gitbooks.io/pecan-documentation/content/" target="_blank">Documentation</a>
      <br>
        <a href="https://join.slack.com/t/pecanproject/shared_invite/enQtMzkyODUyMjQyNTgzLWEzOTM1ZjhmYWUxNzYwYzkxMWVlODAyZWQwYjliYzA0MDA0MjE4YmMyOTFhMjYyMjYzN2FjODE4N2Y4YWFhZmQ" target="_blank">Chat Room</a>
      <br>
        <a href="http://pecanproject.github.io/Report_an_issue.html" target="_blank">Bug Report</a>
      </p>
    </div>
    <div id="output">
