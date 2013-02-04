<?php
#require_once("inc/model/functions.php");
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
$folder = $workflow['folder'];
 
$runFolder = $folder . DIRECTORY_SEPARATOR . "run";
$runs = explode("\n", file_get_contents($runFolder . DIRECTORY_SEPARATOR . "runs.txt"));
// this is an advanced edit, so only one run is supported. use the last one in the file.
$lastRun = $runs[count($runs)-2];

#$dataXml="/home/pecan/pecan/web/sugarcane/default/default.xml";
$dataXml=array_shift(glob($workflow["folder"] . "/run/" . $lastRun . "/config.xml"));

close_database($connection);

// END PEcAn additions

if(isset($_POST["command"]) and strpos($_POST["command"],"continue")!==False){

    // prepare the datafile to be saved
    $dataOrigXml=str_replace("config.xml", "data_orig.xml", $dataXml);

    if (!copy($dataXml, $dataOrigXml)) {
        die("Failed to copy parameters to new file, $dataOrigXml");    
    }

    $doc = new DOMDocument();
    $doc->load($dataXml);
    //$doc->preserveWhiteSpace = false;
    $xpath = new DOMXPath($doc);

    // The name of most of the posted parameters will be an xpath to
    // the same parameter in the config.xml file. Iterate through all the 
    // posted parameters and set the value of the parameter to the posted value.
    foreach($_POST as $key=>$value) {
        // All xpaths for this document will start with /config
        if(strpos($key,"/config") !== false) {
            $query = "/" . $key;
            $nodeList = $xpath->query($query);
            // The above query will only ever return 1 node
            $node = $nodeList->item(0);
            $node->nodeValue = $value;
        }
    }

    if(!$doc->save($dataXml,LIBXML_NOEMPTYTAG)) {
        die("$dataXml could not be saved");
    }

    $dataDiff=str_replace("config.xml", "data.diff", $dataXml);
    exec("diff $dataOrigXml $dataXml > $dataDiff");
    // TODO do something more intelligent with the diff, like save in the database

    // call R code to lauch stage 2 and redirect to running_stage2.php
    chdir($folder);
    pclose(popen('R_LIBS_USER="' . ${pecan_install} . '" R CMD BATCH workflow_stage2.R &', 'r'));
    if ($offline) {
        header( "Location: ../running_stage2.php?workflowid=$workflowid&offline=offline");
    } else {
        header( "Location: ../running_stage2.php?workflowid=$workflowid");
    }           

}

$doc = new DOMDocument();
$doc->load($dataXml);
$rootNode=$doc->documentElement;

$tabs = array();

foreach ($rootNode->childNodes as $tabNode) {
    // filter out top level text nodes
    if ($tabNode->nodeType != 3) {
        if ($tabNode->nodeName != "pft") {
            $tabName = $tabNode->nodeName;
            $childNodes = $tabNode->childNodes;
            $paramNodes=array();
            // filter out text nodes from children
            foreach ($childNodes as $childNode) {
                if ($childNode->nodeType != 3) {
                    $paramNodes[]=$childNode;
                }
            }
            // add this tab and associated parameters to tabs array
            $tabs[]=array($tabName,$paramNodes);
        } else { // these are pft parameters, so we have to go down one level in the tree to create the rest of the tabs
            foreach ($tabNode->childNodes as $pftTabNode) {
                $nodeName = $pftTabNode->nodeName;
                if ($pftTabNode->nodeType != 3 && $nodeName != "comment" && $nodeName != "num") {
                    $tabName = $pftTabNode->nodeName;
                    $childNodes = $pftTabNode->childNodes;
                    $paramNodes=array();
                    // filter out text nodes and comments nodes from children
                    foreach ($childNodes as $childNode) {
                        if ($childNode->nodeType != 3 && $childNode->nodeName != "comment") {
                            $paramNodes[]=$childNode;
                        }
                    }
                    $tabs[] = array($tabName,$paramNodes);
                }
            }
        }
    }
}

include_once("inc/view/main_view.php");
?>
