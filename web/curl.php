<?php
require("common.php");
open_database();

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// get run information
$stmt = $pdo->prepare("SELECT * FROM workflows WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();
$start = substr($workflow['start_date'], 0, 4);
$end = substr($workflow['end_date'], 0, 4);
$folder = $workflow['folder'];
$notes = htmlspecialchars($workflow['notes']);
$params = $workflow['params'];
if ($params == '') {
  die('No parameters found, was this not launched from web interface?');
}
eval('$array = ' . $params . ';');

$pft=$array['pft'];


$stmt = $pdo->prepare("SELECT model_name, revision FROM models WHERE id=?;");
if (!$stmt->execute(array($array['modelid']))) {
  die('Invalid query: ' . error_database());
}
$model = "";
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
    $model = "${row['model_name']} (v${row['revision']})";
}
$stmt->closeCursor();


echo "<pre>\n";
echo "# model       : ${model}\n";
echo "# site        : ${array['sitename']}\n";
echo "# pft         : ${pft[0]}\n";
echo "# time range  : ${array['start']} - ${array['end']}\n";
echo "curl -v -X POST \\\n";
echo "    -F 'hostname=${array['hostname']}' \\\n";
echo "    -F 'modelid=${array['modelid']}' \\\n";
echo "    -F 'sitegroupid=1' \\\n";
echo "    -F 'siteid=${array['siteid']}' \\\n";
echo "    -F 'sitename=${array['sitename']}' \\\n";
echo "    -F 'pft[]=${pft[0]}' \\\n";
echo "    -F 'start=${array['start']}' \\\n";
echo "    -F 'end=${array['end']}' \\\n";
foreach($array as $key => $value) {
    if (substr($key, 0, 6) === "input_" && $value !== "-1") {
        echo "    -F '${key}=${value}' \\\n";
    }
}
echo "    -F 'email=' \\\n";
echo "    -F 'notes=' \\\n";
echo "    'http://localhost:6480/pecan/04-runpecan.php'\n";
echo "</pre>";

close_database();
