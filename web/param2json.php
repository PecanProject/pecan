<?php
require("common.php");
open_database();

$query = "select cast(floor(nextval('users_id_seq') / 1e9) as bigint) AS id";
$stmt = $pdo->query($query);
$row = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();
if (array_key_exists('id', $row)) {
    $where = "AND workflows.id >= {$row['id']}000000000 AND workflows.id <= {$row['id']}999999999";
} else {
    $where = "AND workflows.id >= 99000000000 AND workflows.id <= 99999999999";
}

$query = "SELECT workflows.id, workflows.params, attributes.value FROM workflows LEFT OUTER JOIN attributes ON workflows.id=attributes.container_id AND attributes.container_type='workflows' WHERE params != '' AND value is null {$where} ORDER BY workflows.id DESC";
$stmt = $pdo->prepare($query);
if ($stmt->execute() === FALSE) {
  die('Invalid query: ' . error_database());
}

$ignore_vars = array("Pycharm-abdee044","Pycharm-abdee403","Phpstorm-bd36376b","_ga","Pycharm-abdee404","__utmz","Pycharm-abdee405","cloud-user","refresh","PHPSESSID","__utmc","user-id","csrf-token","_bety_session_id","__utma","portainer_pagination_containers","portainer_pagination_images","id","mmsa-hosted","m");

print "<pre>";

while ($row = $stmt->fetch(PDO::FETCH_ASSOC)) {
    $params = eval("return {$row['params']};");
    foreach ($ignore_vars as $x) {
        unset($params[$x]);
    }
    $json = str_replace("\\/", "/", json_encode($params));

    $ins = $pdo->prepare("INSERT INTO attributes(container_type, container_id, value) VALUES ('workflows', :p2, :p3);");
    $ins->bindParam(':p2', $row['id'], PDO::PARAM_INT);
    $ins->bindParam(':p3', $json, PDO::PARAM_STR);
    if ($ins->execute() === FALSE) {
      die('Invalid query: ' . error_database());
    }

    $pdo->exec("UPDATE workflows SET params = '' WHERE id = {$row['id']}");
    print("updated workflow with id={$row['id']}</br>");
}

print "</pre>";

close_database();
?>

