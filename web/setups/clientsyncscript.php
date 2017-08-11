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

// // Create map with request parameters
// $params = array ('client_sceret' => $client_sceret, 'server_auth_token' => $server_auth_token, 'fqdn' => $fqdn );
//
// // Build Http query using params
// $query = http_build_query ($params);
//
// // Create Http context details
// $contextData = array (
//                 'method' => 'POST',
//                 'header' => "Connection: close\r\n".
//                             "Content-Length: ".strlen($query)."\r\n",
//                 'content'=> $query );
//
// // Create context resource for our request
// $context = stream_context_create (array ( 'http' => $contextData ));
//
// // Read page rendered as result of your POST request
// $result =  file_get_contents (
//                   $server_url,  // page url
//                   false,
//                   $context);

//
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
    die('error occured during curl exec. Additioanl info: ' . var_export($info));
}

// close curl
curl_close($curl);
$decoded = json_decode($curl_response, FALSE);
if (isset($decoded->status) && $decoded->status == 'ERROR') {
    die('error occured: ' . $decoded->errormessage);
}

// got wait id

var_dump($decoded);

echo $decoded->wantid;


//var_export($decoded['waitid']);

// script to handle wait id part
?>
