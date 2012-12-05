<?php
require_once("inc/model/functions.php");
require_once("config/file_locations.php");
require_once("config/graph_variables.php");

// START PEcAn additions

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// database parameters
require("../dbinfo.php");
$connection=open_database();

// get run information
$query = "SELECT folder FROM workflows WHERE workflows.id=$workflowid";
$result = mysql_query($query);
if (!$result) {
    die('Invalid query: ' . mysql_error());
}
$workflow = mysql_fetch_assoc($result);

// datafile
// TODO fix run folder name by looking at $workflow["folder"] . "/run/runs.txt"
$default_xml=array_shift(glob($workflow["folder"] . "/run/*/data.xml"));

// load default values
// TODO check with daeyun about get_default function
get_default($default_xml);

// set the datafile to be saved
$new_xml_file=str_replace("data.xml", "data_modified.xml", $default_xml);

close_database($connection);

// END PEcAn additions

if(isset($_POST["command"]) and strpos($_POST["command"],"create_xml")!==False){
    error_reporting(0);
    $xml_string=generate_xml($_POST,$xml_structure_file);
    $f = fopen($new_xml_file, 'w');
    if($f==False){
        echo "$new_xml_file is not writable";
        exit();
    }
    fwrite($f, $xml_string);
    chmod("$new_xml_file", 0666);
    fclose($f);
    echo "successful";
    // TODO check if file is modified and mark in database
    // diff -b $default_xml $new_xml_file

    // TODO save new configuration to run folder
    // move $new_xml_file $default_xml

    // TODO call R code to lauch stage 2 and redirect to running_stage2.php
//            chdir($folder);
//            pclose(popen('R_LIBS_USER="' . ${pecan_install} . '" R CMD BATCH workflow_stage2.R &', 'r'));
//            if ($offline) {
//                header( "Location: ../running_stage2.php?workflowid=$workflowid&offline=offline");
//            } else {
//                header( "Location: ../running_stage2.php?workflowid=$workflowid");
//            }           

    exit();
}else if(isset($_POST["command"]) and $_POST["command"]=="run"){
 
    $result=shell_exec("sh ".$sh_file);

    if(strpos($result, "done")!==False){
        echo "successful";
    }else{
        echo "$sh_file could not be executed. Try changing the persmission of the file to 755.";
    }
    exit();
}else if(isset($_GET["command"]) and $_GET["command"]=="default"){
    echo get_default($default_xml);
    exit();
}else if(isset($_POST["command"]) and $_POST["command"]=="plot"){
    echo shell_exec("./python/png/delete_old.sh > /dev/null 2>/dev/null &");
    $python_file="./python/plot.py";

    $var1=strict_sanitize($_POST["var_group_x"]);
    $var2=strict_sanitize($_POST["var_group_y"]);
    $python_args=$var1." ".$var2;

    $result=exec("python ".$python_file.' '.$python_args);
    if(strpos($result, ".png")!==False){
        echo trim($result);
    }else{
        echo "$python_file could not be executed. Try changing the persmission of the file to 755.";
    }
    exit();
}

$items=read_xml_structure($xml_structure_file);
include_once("inc/view/main_view.php");
?>
