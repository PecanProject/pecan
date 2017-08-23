<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// client side script

include_once '../config.php';

$service_url = $server_url."/pecan/setups/serversyncscript.php";

$curl = curl_init($service_url);

$curl_post_data = array ('client_sceret' => $client_sceret,
                         'server_auth_token' => $server_auth_token,
                         'fqdn' => $fqdn );

// setting curl to do a POST request
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
curl_setopt($curl, CURLOPT_POST, true);
curl_setopt($curl, CURLOPT_POSTFIELDS, $curl_post_data);

// execute the curl request
$curl_response = curl_exec($curl);

// if curls execution fails
if ($curl_response === false) {
    $info = curl_getinfo($curl);
    curl_close($curl);
    die('error occured during curl exec. Additional info: ' . var_export($info));
}

// close curl
curl_close($curl);
$decoded = json_decode($curl_response, FALSE);
if (isset($decoded->status) && $decoded->status == 'ERROR') {
    die('error occured: ' . $decoded->errormessage);
}

// instructions to update the client secrets
if (!isset($client_sceret) && empty($client_sceret)) {
  $curl_post_data = array ('client_sceret' => $client_sceret);
  $service_url = $server_url."/pecan/setups/add.php";
  $curl = curl_init($service_url);
  // set the curl to do POST request to add client secret to the config page
  curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl, CURLOPT_POST, true);
  curl_setopt($curl, CURLOPT_POSTFIELDS, $curl_post_data);
  // execute the curl request
  $curl_response = curl_exec($curl);
}

// script to handle wait id part
//echo $decoded->wantid;

$tempfile = tmpfile();
$line = $decoded->wantid;
fwrite($tempfile, $line);

$configfile = fopen("syncflag.txt", "w+");

rewind($tempfile);

while (($buffer=fgets($tempfile))!== false) {
  fwrite($configfile,$buffer);
}

fclose($tempfile); // remove tempfile

?>
