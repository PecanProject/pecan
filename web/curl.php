<?php

function getURL() {
    $url = "";

    if (array_key_exists('HTTP_X_FORWARDED_PROTO', $_SERVER)) {
        $url = $_SERVER['HTTP_X_FORWARDED_PROTO'];
    } elseif (array_key_exists('REQUEST_SCHEME', $_SERVER)) {
        $url = $_SERVER['REQUEST_SCHEME'];
    } else {
        $url = "http";
    }

    $url .= "://";

    if (array_key_exists('HTTP_X_FORWARDED_HOST', $_SERVER)) {
        $url .= $_SERVER['HTTP_X_FORWARDED_HOST'];
    } else {
        if (array_key_exists('SERVER_NAME', $_SERVER)) {
            $url .= $_SERVER['HTTP_X_FORWARDED_HOST'];
        } else {
            $url .= "localhost";
        }
        if (array_key_exists('SERVER_PORT', $_SERVER)) {
            $url .= ":{$_SERVER['SERVER_PORT']}";
        }
    }

    if (array_key_exists('SCRIPT_NAME', $_SERVER)) {
        $url .= str_replace("/curl.php", "/04-runpecan.php", $_SERVER['SCRIPT_NAME']);
    } else {
        $url .= "/pecan/04-runpecan.php";
    }

    return $url;
}

require("common.php");
open_database();

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// get run information
$stmt = $pdo->prepare("SELECT * FROM workflows LEFT OUTER JOIN attributes ON workflows.id=attributes.container_id AND attributes.container_type='workflows' WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();
$start = substr($workflow['start_date'], 0, 4);
$end = substr($workflow['end_date'], 0, 4);
$folder = $workflow['folder'];
$notes = htmlspecialchars($workflow['notes']);

if ($workflow['value'] != '') {
  $params = json_decode($workflow['value'], true);
} else {
  die("Missing parameters for workflow, was this launched from the web interface?");
}

$pft=$params['pft'];


$stmt = $pdo->prepare("SELECT model_name, revision FROM models WHERE id=?;");
if (!$stmt->execute(array($params['modelid']))) {
  die('Invalid query: ' . error_database());
}
$model = "";
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
    $model = "{$row['model_name']} (v{$row['revision']})";
}
$stmt->closeCursor();


echo "<pre>\n";
echo "# model       : {$model}\n";
echo "# site        : {$params['sitename']}\n";
echo "# pft         : {$pft[0]}\n";
echo "# time range  : {$params['start']} - {$params['end']}\n";
echo "curl -v -X POST \\\n";
echo "    -F 'hostname={$params['hostname']}' \\\n";
echo "    -F 'modelid={$params['modelid']}' \\\n";
echo "    -F 'sitegroupid=1' \\\n";
echo "    -F 'siteid={$params['siteid']}' \\\n";
echo "    -F 'sitename={$params['sitename']}' \\\n";
echo "    -F 'pft[]={$pft[0]}' \\\n";
echo "    -F 'start={$params['start']}' \\\n";
echo "    -F 'end={$params['end']}' \\\n";
foreach($params as $key => $value) {
    if (substr($key, 0, 6) === "input_" && $value !== "-1") {
        echo "    -F '{$key}={$value}' \\\n";
    }
}
echo "    -F 'email={$params['email']}' \\\n";
echo "    -F 'notes={$params['notes']}' \\\n";
echo "    '" . getURL() . "'\n";
echo "</pre>";

close_database();
