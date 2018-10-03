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
if ($authentication) {
    if (!check_login()) {
        close_database();
        header('HTTP/1.1 403 Unauthorized');
        exit;
    }
    if (get_page_acccess_level() > $min_run_level) {
        header( "Location: history.php");
        close_database();
        exit;
    }
}

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

if (!isset($_REQUEST['type'])) {
  die("Need type.");
}
$type=$_REQUEST['type'];

// get run information
$stmt = $pdo->prepare("SELECT folder FROM workflows WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$run = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();
$folder = str_replace("//", "/", $run['folder']);

// return dataset
switch ($type) {
    case "file":
        if (!isset($_REQUEST['name'])) {
            die("Need name.");
        }
        $name = $_REQUEST['name'];
        
        $file = canonicalize("$folder/$name");
        if (substr($file, 0, strlen($folder)) != $folder) {
            die("Invalid file name specified.");            
        }
        
        if (substr($name, -4) === ".xml") {
            $mime = "text/xml";
        } else if (substr($name, -4) === ".txt") {
            $mime = "text/plain";                    
        } else if (substr($name, -4) === ".log") {
            $mime = "text/plain";                    
        } else if (substr($name, -4) === ".pdf") {
            $mime = "application/pdf";                    
        } else {
            $mime = "application/octet-stream";
        }
        break;
        
    case "plot":
        if (!isset($_REQUEST['run'])) {
            die("Need run.");
        }
        $run = $_REQUEST['run'];
        if (!isset($_REQUEST['year']) || !is_numeric($_REQUEST['year'])) {
            die("Need year.");
        }
        $year = $_REQUEST['year'];
        if (!isset($_REQUEST['xvar'])) {
            die("Need xvar.");
        }
        $xvar = $_REQUEST['xvar'];
        if (!isset($_REQUEST['yvar'])) {
            die("Need yvar.");
        }
        $yvar = $_REQUEST['yvar'];
        $width = 600;
        if (isset($_REQUEST['width']) && ($_REQUEST['width'] > $width)) {
            $width = $_REQUEST['width'];
        }
        $height = 400;
        if (isset($_REQUEST['height']) && ($_REQUEST['height'] > $height)) {
            $height = $_REQUEST['height'];
        }
        $datafile = $folder . "/out/" . $run . "/" . $year . ".nc";
        $mime = "image/png";
        $file = tempnam(sys_get_temp_dir(),'plot') . ".png";
        if (!file_exists($datafile)) {
            die("Invalid file name specified ${file}.");            
        }

        # execute command to create graph
        $escapedargs = escapeshellarg("--args $datafile $year $xvar $yvar $width $height $file");
        shell_exec("R_LIBS_USER='${R_library_path}' PECANSETTINGS='$folder/pecan.xml' ${Rbinary} CMD BATCH --vanilla $escapedargs plot.netcdf.R /tmp/plot.out");
        break;
        
    default:
        die("unknown type.");
}

if (!file_exists($file)) {
    die("Invalid file name specified ${file}.");            
}
if ($mime != "") {
    header("Content-type: $mime");
}
if (isset($name)) {
    header('Content-Disposition: filename='.basename($name));
}
readfile($file);

if ($type == "plot") {
  unlink($file);
}

function canonicalize($address)
{
    $address = explode('/', $address);
    $keys = array_keys($address, '..');

    foreach($keys AS $keypos => $key)
    {
        array_splice($address, $key - ($keypos * 2 + 1), 2);
    }

    $address = implode('/', $address);
    $address = str_replace('./', '', $address);

    return $address;
}
?>
