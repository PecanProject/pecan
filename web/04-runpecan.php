<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
require("common.php");

open_database();
if ($authentication) {
  if (!check_login()) {
		header( "Location: index.php");
		close_database();
		exit;
	}
    if (get_page_acccess_level() > $min_run_level) {
        header( "Location: history.php");
        close_database();
        exit;
    }
}

# tunnel options
if (isset($_REQUEST['username'])) {
  $tunnel_username = $_REQUEST['username'];
  unset($_REQUEST['username']);
} else {
  $tunnel_username = "";
}
if (isset($_REQUEST['password'])) {
  $tunnel_password = $_REQUEST['password'];
  unset($_REQUEST['password']);
} else {
  $tunnel_password = "";
}

# boolean parameters
$userok=isset($_REQUEST['userok']);
$offline=isset($_REQUEST['offline']);
$pecan_edit=isset($_REQUEST['pecan_edit']);
$model_edit=isset($_REQUEST['model_edit']);
$qsub=isset($_REQUEST['qsub']);

# parameters
if (!isset($_REQUEST['siteid'])) {
  die("Need a siteid.");
}
$siteid=$_REQUEST['siteid'];

if (!isset($_REQUEST['modelid'])) {
  die("Need a modelid.");
}
$modelid=$_REQUEST['modelid'];

if (!isset($_REQUEST['hostname'])) {
  die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
if (!array_key_exists($hostname, $hostlist)) {
  die("{$hostname} is not an approved host");
}
$hostoptions = $hostlist[$hostname];

if (!isset($_REQUEST['pft'])) {
	die("Need a pft.");
}
$pft=array_unique($_REQUEST['pft']);

# dates
if (!isset($_REQUEST['start'])) {
  die("Need a start date.");
}
$startdate=$_REQUEST['start'];
$metstart=$startdate;

if (!isset($_REQUEST['end'])) {
  die("Need a end date.");
}
$enddate=$_REQUEST['end'];
$metend=$enddate;

# non required parameters
$email = "";
if (isset($_REQUEST['email'])) {
  $email = $_REQUEST['email'];
}
$runs = "1";
if (isset($_REQUEST['runs'])) {
  $runs = $_REQUEST['runs'];
}
$variables = "NPP";
if (isset($_REQUEST['variables'])) {
  $variables = $_REQUEST['variables'];
}
$parm_method = "uniform";
if (isset($_REQUEST['parm_method'])) {
  $parm_method = $_REQUEST['parm_method'];
}
$notes_xml = "";
$notes_db = "";
if (isset($_REQUEST['notes'])) {
  $notes_xml = $_REQUEST['notes'];
  $notes_db = html_entity_decode($_REQUEST['notes']);
}
$sensitivity = array();
if (isset($_REQUEST['sensitivity'])) {
    $sensitivity = array_filter(explode(",",$_REQUEST['sensitivity']),'strlen');
}

# check met info
if (isset($_REQUEST['input_met']) && is_numeric($_REQUEST['input_met'])) {
  $stmt = $pdo->prepare("SELECT start_date, end_date FROM inputs WHERE inputs.id=?");
  if (!$stmt->execute(array($_REQUEST['input_met']))) {
    die('Invalid query: ' . (error_database()));
  }
  $row = $stmt->fetch(PDO::FETCH_ASSOC);
  $metstart=$row['start_date'];
  $metend=$row['end_date'];
  $stmt->closeCursor();
}

// Set user and runtime
$runtime = gmdate('Y/m/d H:i:s O'); 
$metstart2 = date("Y/m/d", strtotime($metstart));
$metend2   = date("Y/m/d", strtotime($metend));

// check input dates to make sure they agree with the dates from the weather data
if (!$userok && ($startdate < $metstart2 || $enddate > $metend2)) {
	$params = "userok=on";
	foreach($_REQUEST as $k => $v) {
		if (is_array($v)) {
			foreach($v as $x) {
				$params .= "&{$k}[]=$x";
			}
		} else {
		  if(strcmp($k, "notes") == 0) {
			$str = htmlentities($v, ENT_QUOTES);
			$params .= "&{$k}=$str";
		  } else {
			$params .= "&{$k}=$v";
		  }
		}
	}
	$params .= "&msg=WARNING : Selected dates are not within the bounds of the weather data file you selected.  START: {$startdate} {$metstart2}   END: {$enddate} {$metend2}";
	header("Location: checkfailed.php?{$params}");
	exit();
}

// get model info
$stmt = $pdo->prepare("SELECT name, revision FROM models, modeltypes WHERE models.id=? AND modeltypes.id=models.modeltype_id");
if (!$stmt->execute(array($modelid))) {
  die('Invalid query: ' . (error_database()));
}
$row = $stmt->fetch(PDO::FETCH_ASSOC);
$revision=$row['revision'];
$modeltype=$row['name'];
$stmt->closeCursor();

// create the workflow execution
$userid=get_userid();
if ($userid != -1) {
  $q=$pdo->prepare("INSERT INTO workflows (site_id, model_id, notes, folder, hostname, start_date, end_date, advanced_edit, started_at, user_id) values (:siteid, :modelid, :notes, '', :hostname, :startdate, :enddate, :advanced_edit, NOW(), :userid)");
} else {
  $q=$pdo->prepare("INSERT INTO workflows (site_id, model_id, notes, folder, hostname, start_date, end_date, advanced_edit, started_at) values (:siteid, :modelid, :notes, '', :hostname, :startdate, :enddate, :advanced_edit, NOW())");
}
$q->bindParam(':siteid', $siteid, PDO::PARAM_INT);
$q->bindParam(':modelid', $modelid, PDO::PARAM_INT);
$q->bindParam(':notes', $notes_db, PDO::PARAM_STR);
$q->bindParam(':hostname', $hostname, PDO::PARAM_STR);
$q->bindParam(':startdate', $startdate, PDO::PARAM_STR);
$q->bindParam(':enddate', $enddate, PDO::PARAM_STR);
if ($userid != -1) {
  $q->bindParam(':userid', $userid, PDO::PARAM_INT);
}
$advanced_edit = ($pecan_edit || $model_edit) ? "yes" : "no";
$q->bindParam(':advanced_edit', $advanced_edit, PDO::PARAM_INT);
if ($q->execute() === FALSE) {
  die('Can\'t insert workflow : ' . (error_database()));
}
if ($db_bety_type == 'pgsql') {
  $workflowid=$pdo->lastInsertId('workflows_id_seq');
} else {
  $workflowid=$pdo->lastInsertId();
}

# folders
$folder = $output_folder . DIRECTORY_SEPARATOR . 'PEcAn_' . $workflowid;
if ($pdo->query("UPDATE workflows SET folder='{$folder}' WHERE id={$workflowid}") === FALSE) {
  die('Can\'t update workflow : ' . (error_database()));
}

# parameters
$params=json_encode($_REQUEST);
$ins = $pdo->prepare("INSERT INTO attributes(container_type, container_id, value) VALUES ('workflows', :id, :params);");
$ins->bindParam(':id', $workflowid, PDO::PARAM_INT);
$ins->bindParam(':params', $params, PDO::PARAM_STR);
if ($ins->execute() === FALSE) {
  die('Can\'t insert attributes : ' . (error_database()));
}

# quick check on dbfiles_folder
if (! isset($dbfiles_folder)) {
  if (isset($inputs_folder)) {
    $dbfiles_folder = $inputs_folder;
  } else {
    $dbfiles_folder = $output_folder . DIRECTORY_SEPARATOR . "dbfiles";
  }
}

# setup umask so group has write as well
umask(0000);

# create the folder(s)
if (!mkdir($folder)) {
    die("Can't create output folder [{$folder}]");
}
if (!is_dir($dbfiles_folder) && !mkdir($dbfiles_folder, 0777, true)) {
    die("Can't create output folder [{$dbfiles_folder}]");
}
if ($hostname != $fqdn) {
    $tunnel_folder = $folder . DIRECTORY_SEPARATOR . "tunnel";
    if (!mkdir($tunnel_folder)) {
        die("Can't create output folder [{$tunnel_folder}]");
    }
    
    ## data tunnel
    if(isset($hostoptions['data_hostname'])){
        $data_tunnel_folder = $tunnel_folder . DIRECTORY_SEPARATOR . "data"; 
        if (!mkdir($data_tunnel_folder)) {
            die("Can't create output folder [{$data_tunnel_folder}]");
        }
    }
}

# create pecan.xml
$fh = fopen($folder . DIRECTORY_SEPARATOR . "pecan.xml", 'w');
fwrite($fh, "<?xml version=\"1.0\"?>" . PHP_EOL);
fwrite($fh, "<pecan>" . PHP_EOL);

fwrite($fh, "  <info>" . PHP_EOL);
fwrite($fh, "    <notes>" . toXML($notes_xml) . "</notes>" . PHP_EOL);
fwrite($fh, "    <userid>" . get_userid() . "</userid>" . PHP_EOL);
fwrite($fh, "    <username>" . get_user_name() . "</username>" . PHP_EOL);
fwrite($fh, "    <date>{$runtime}</date>" . PHP_EOL);
fwrite($fh, "  </info>" . PHP_EOL);

fwrite($fh, "  <outdir>{$folder}</outdir>" . PHP_EOL);

fwrite($fh, "  <database>" . PHP_EOL);

fwrite($fh, "    <bety>" . PHP_EOL);
fwrite($fh, "      <user>{$db_bety_username}</user>" . PHP_EOL);
fwrite($fh, "      <password>{$db_bety_password}</password>" . PHP_EOL);
fwrite($fh, "      <host>{$db_bety_hostname}</host>" . PHP_EOL);
if (isset($db_bety_port)) {
        fwrite($fh, "      <port>{$db_bety_port}</port>" . PHP_EOL);
}
fwrite($fh, "      <dbname>{$db_bety_database}</dbname>" . PHP_EOL);
fwrite($fh, "      <driver>PostgreSQL</driver>" . PHP_EOL);
fwrite($fh, "      <write>true</write>" . PHP_EOL);
fwrite($fh, "    </bety>" . PHP_EOL);

if (isset($db_fia_database) && ($db_fia_database != "")) {
	fwrite($fh, "    <fia>" . PHP_EOL);
	fwrite($fh, "      <user>{$db_fia_username}</user>" . PHP_EOL);
	fwrite($fh, "      <password>{$db_fia_password}</password>" . PHP_EOL);
	fwrite($fh, "      <host>{$db_fia_hostname}</host>" . PHP_EOL);
        if (isset($db_fia_port)) {
                fwrite($fh, "      <port>{$db_fia_port}</port>" . PHP_EOL);
        }
	fwrite($fh, "      <dbname>{$db_fia_database}</dbname>" . PHP_EOL);
	if ($db_fia_type == "mysql") {
		fwrite($fh, "      <driver>MySQL</driver>" . PHP_EOL);
	} else if ($db_fia_type = "pgsql") {
		fwrite($fh, "      <driver>PostgreSQL</driver>" . PHP_EOL);
	}
	fwrite($fh, "    </fia>" . PHP_EOL);
}

fwrite($fh, "    <dbfiles>{$dbfiles_folder}</dbfiles>" . PHP_EOL);
fwrite($fh, "  </database>" . PHP_EOL);


fwrite($fh, "  <pfts>" . PHP_EOL);
foreach($pft as $p) {
	fwrite($fh, "    <pft>" . PHP_EOL);
	fwrite($fh, "      <name>{$p}</name> " . PHP_EOL);
	fwrite($fh, "    </pft>" . PHP_EOL);
}
fwrite($fh, "  </pfts>" . PHP_EOL);

fwrite($fh, "  <meta.analysis>" . PHP_EOL);
fwrite($fh, "    <iter>3000</iter>" . PHP_EOL);
fwrite($fh, "    <random.effects>" . PHP_EOL);
fwrite($fh, "     <on>FALSE</on>" . PHP_EOL);
fwrite($fh, "     <use_ghs>TRUE</use_ghs>" . PHP_EOL);
fwrite($fh, "    </random.effects>" . PHP_EOL);
fwrite($fh, "  </meta.analysis>" . PHP_EOL);

if (!empty($runs)){
	fwrite($fh, "  <ensemble>" . PHP_EOL);
	fwrite($fh, "   <size>{$runs}</size>" . PHP_EOL);
	fwrite($fh, "   <variable>{$variables}</variable>" . PHP_EOL);
	fwrite($fh, "   <samplingspace>" . PHP_EOL);
	fwrite($fh, "   <parameters>" . PHP_EOL);
	fwrite($fh, "    <method>{$parm_method}</method>" . PHP_EOL);
	fwrite($fh, "   </parameters>" . PHP_EOL);
	fwrite($fh, "   <met>" . PHP_EOL);
	fwrite($fh, "    <method>sampling</method>" . PHP_EOL);
    	fwrite($fh, " 	</met>" . PHP_EOL);
	fwrite($fh, "   </samplingspace>" . PHP_EOL);
	fwrite($fh, "  </ensemble>" . PHP_EOL);
} else {
	fwrite($fh, "  <ensemble>" . PHP_EOL);
	fwrite($fh, "    <size>1</size>" . PHP_EOL);
	fwrite($fh, "    <variable>NPP</variable>" . PHP_EOL);
	fwrite($fh, "    <samplingspace>" . PHP_EOL);
	fwrite($fh, "     <parameters>" . PHP_EOL);
	fwrite($fh, "       <method>uniform</method>" . PHP_EOL);
	fwrite($fh, "     </parameters>" . PHP_EOL);
	fwrite($fh, "     <met>" . PHP_EOL);
	fwrite($fh, "       <method>sampling</method>" . PHP_EOL);
   	 fwrite($fh, "    </met>" . PHP_EOL);
	fwrite($fh, "    </samplingspace>" . PHP_EOL);
	fwrite($fh, "  </ensemble>" . PHP_EOL);
}

if (!empty($sensitivity)) {
	fwrite($fh, "  <sensitivity.analysis>" . PHP_EOL);
	fwrite($fh, "    <quantiles>" . PHP_EOL);
	foreach($sensitivity as $s) {
		fwrite($fh, "      <sigma>{$s}</sigma>" . PHP_EOL);
	}
	fwrite($fh, "    </quantiles>" . PHP_EOL);
	fwrite($fh, "    <variable>{$variables}</variable>" . PHP_EOL);
	fwrite($fh, "  </sensitivity.analysis>" . PHP_EOL);
}

fwrite($fh, "  <model>" . PHP_EOL);
fwrite($fh, "    <id>{$modelid}</id>" . PHP_EOL);
if ($modeltype == "ED2") {
  fwrite($fh, "    <edin>ED2IN.r{$revision}</edin>" . PHP_EOL);
	fwrite($fh, "    <config.header>" . PHP_EOL);
	fwrite($fh, "      <radiation>" . PHP_EOL);
	fwrite($fh, "        <lai_min>0.01</lai_min>" . PHP_EOL);
	fwrite($fh, "      </radiation>" . PHP_EOL);
	fwrite($fh, "      <ed_misc>" . PHP_EOL);
	fwrite($fh, "        <output_month>12</output_month>      " . PHP_EOL);
	fwrite($fh, "      </ed_misc> " . PHP_EOL);
	fwrite($fh, "    </config.header>" . PHP_EOL);
	fwrite($fh, "    <phenol.scheme>0</phenol.scheme>" . PHP_EOL);
}
if (isset($hostoptions['models'])) {
  $model_version="{$modeltype}";
  if (isset($hostoptions['models']["{$modeltype} (r{$revision})"])) {
    $model_version="{$modeltype} (r{$revision})";
  }
  if (isset($hostoptions['models'][$model_version])) {
    if (is_array($hostoptions['models'][$model_version])) {
      if (isset($hostoptions['models'][$model_version]['prerun'])) {
        fwrite($fh, "    <prerun>" . toXML($hostoptions['models'][$model_version]['prerun']) . "</prerun>" . PHP_EOL);      
      }
      if (isset($hostoptions['models'][$model_version]['postrun'])) {
        fwrite($fh, "    <postrun>" . toXML($hostoptions['models'][$model_version]['postrun']) . "</postrun>" . PHP_EOL);      
      }
    } else {
      fwrite($fh, "    <prerun>" . toXML($hostoptions['models'][$model_version]) . "</prerun>" . PHP_EOL);      
    }
  }
}
fwrite($fh, "  </model>" . PHP_EOL);
fwrite($fh, "  <workflow>" . PHP_EOL);
fwrite($fh, "    <id>$workflowid</id>" . PHP_EOL);
fwrite($fh, "  </workflow>" . PHP_EOL);
fwrite($fh, "  <run>" . PHP_EOL);
fwrite($fh, "    <site>" . PHP_EOL);
fwrite($fh, "      <id>{$siteid}</id>" . PHP_EOL);
fwrite($fh, "      <met.start>{$metstart}</met.start>" . PHP_EOL);
fwrite($fh, "      <met.end>{$metend}</met.end>" . PHP_EOL);
fwrite($fh, "    </site>" . PHP_EOL);
fwrite($fh, "    <inputs>" . PHP_EOL);
foreach($_REQUEST as $key => $val) {
  if (substr($key, 0, 6) != "input_") continue;
  if ($val == -1) continue;
  $tag=substr($key, 6);
  fwrite($fh, "      <{$tag}>" . PHP_EOL);
  if (is_numeric($val)) {
    fwrite($fh, "        <id>{$val}</id>" . PHP_EOL);
  } else {
    $parts=explode(".", $val, 3);
    fwrite($fh, "        <source>{$parts[0]}</source>" . PHP_EOL);
    fwrite($fh, "        <output>{$parts[1]}</output>" . PHP_EOL);
    if (count($parts) > 2) {
      fwrite($fh, "        <product>{$parts[2]}</product>" . PHP_EOL);
    }
    if (isset($_REQUEST['fluxusername'])) {
      fwrite($fh, "      <username>{$_REQUEST['fluxusername']}</username>" . PHP_EOL);
    }
  }
  fwrite($fh, "      </{$tag}>" . PHP_EOL);
}
fwrite($fh, "    </inputs>" . PHP_EOL);
fwrite($fh, "    <start.date>{$startdate}</start.date>" . PHP_EOL);
fwrite($fh, "    <end.date>{$enddate}</end.date>" . PHP_EOL);
fwrite($fh, "  </run>" . PHP_EOL);

fwrite($fh, "  <host>" . PHP_EOL);
if ($hostname == $fqdn) {
  fwrite($fh, "    <name>localhost</name>" . PHP_EOL);
} else {
  fwrite($fh, "    <name>{$hostname}</name>" . PHP_EOL);
}
if ($tunnel_username != "") {
  fwrite($fh, "    <user>{$tunnel_username}</user>" . PHP_EOL);
}
if (isset($hostoptions['folder'])) {
  $remote = $hostoptions['folder'];
  if ($tunnel_username != "") {
    $remote = $remote . "/" . $tunnel_username;
  }
  fwrite($fh, "    <folder>" . toXML($remote) . "</folder>" . PHP_EOL);
}
if (isset($hostoptions['scratchdir'])) {
  fwrite($fh, "    <scratchdir>" . toXML($hostoptions['scratchdir']) . "</scratchdir>" . PHP_EOL);
}
if (isset($hostoptions['prerun'])) {
  fwrite($fh, "    <prerun>" . toXML($hostoptions['prerun']) . "</prerun>" . PHP_EOL);
}
if (isset($hostoptions['postrun'])) {
  fwrite($fh, "    <postrun>" . toXML($hostoptions['postrun']) . "</postrun>" . PHP_EOL);
}
if (isset($hostoptions['qsub'])) {
  fwrite($fh, "    <qsub>" . toXML($hostoptions['qsub']) . "</qsub>" . PHP_EOL);
}
if (isset($hostoptions['jobid'])) {
  fwrite($fh, "    <qsub.jobid>" . toXML($hostoptions['jobid']) . "</qsub.jobid>" . PHP_EOL);
}
if (isset($hostoptions['qstat'])) {
  fwrite($fh, "    <qstat>" . toXML($hostoptions['qstat']) . "</qstat>" . PHP_EOL);
}
if (isset($hostoptions['Rbinary'])) {
  fwrite($fh, "    <Rbinary>" . toXML($hostoptions['Rbinary']) . "</Rbinary>" . PHP_EOL);
}
if (isset($hostoptions['job.sh'])) {
  fwrite($fh, "    <job.sh>" . toXML($hostoptions['job.sh']) . "</job.sh>" . PHP_EOL);
}
if ($hostname != $fqdn) {
  fwrite($fh, "    <tunnel>" . $tunnel_folder . DIRECTORY_SEPARATOR . "tunnel" . "</tunnel>" . PHP_EOL);
  if(isset($hostoptions['data_hostname'])){
    fwrite($fh, "    <data_tunnel>" . $data_tunnel_folder . DIRECTORY_SEPARATOR . "tunnel" . "</data_tunnel>" . PHP_EOL);
    fwrite($fh, "    <data_hostname>" . toXML($hostoptions['data_hostname']) . "</data_hostname>" . PHP_EOL);
  }
}
if (isset($hostoptions['rabbitmq_uri'])) {
  $rabbitmq_uri = $hostoptions['rabbitmq_uri'];
  $rabbitmq_model_queue = $modeltype . "_" . $revision;

  fwrite($fh, "    <rabbitmq>" . PHP_EOL);
  fwrite($fh, "      <uri>" . $rabbitmq_uri . "</uri>" . PHP_EOL);
  fwrite($fh, "      <queue>" . $rabbitmq_model_queue . "</queue>" . PHP_EOL);
  fwrite($fh, "    </rabbitmq>" . PHP_EOL);
}
fwrite($fh, "  </host>" . PHP_EOL);

if ($email != "") {
	$url = isset($_SERVER['HTTPS']) ? "https://" : "http://";
	$url .= $_SERVER['HTTP_HOST'] . ':' . $_SERVER['SERVER_PORT'];
	$url .= str_replace("04-runpecan.php", "08-finished.php", $_SERVER["SCRIPT_NAME"]);
	if ($offline) {
		$url .= "?workflowid={$workflowid}&offline=offline";
	} else {
		$url .= "?workflowid={$workflowid}";
	}
	fwrite($fh, "  <email>" . PHP_EOL);
  fwrite($fh, "    <to>{$email}</to>" . PHP_EOL);
  fwrite($fh, "    <url>{$url}</url>" . PHP_EOL);
	fwrite($fh, "  </email>" . PHP_EOL);
}
fwrite($fh, "</pecan>" . PHP_EOL);
fclose($fh);

# copy workflow
copy("workflow.R", "{$folder}/workflow.R");

# create the tunnel
if ($hostname != $fqdn) {
    # write pasword
    $fh = fopen($tunnel_folder . DIRECTORY_SEPARATOR . "password", 'w');
    fwrite($fh, $tunnel_password . PHP_EOL);
    fclose($fh);

    # start tunnel
    pclose(popen("{$SSHtunnel} {$hostname} {$tunnel_username} {$tunnel_folder} > {$tunnel_folder}/log &", 'r'));
    
    ## data tunnel
    if(isset($hostoptions['data_hostname'])){
        # write password
        $fh = fopen($data_tunnel_folder . DIRECTORY_SEPARATOR . "password", 'w');
        fwrite($fh, $tunnel_password . PHP_EOL);
        fclose($fh);

        # start tunnel
        pclose(popen("{$SSHtunnel} {$hostoptions['data_hostname']} {$tunnel_username} {$data_tunnel_folder} > {$data_tunnel_folder}/log &", 'r'));
    
    }
}

# redirect to the right location
if ($pecan_edit) {
  $path = "06-edit.php?workflowid=$workflowid&pecan_edit=pecan_edit&hostname={$hostname}";
  if ($model_edit) {
    $path .= "&model_edit=model_edit";
  }
  if ($offline) {
    $path .= "&offline=offline";
  }
  header("Location: {$path}");
} else if (isset($hostoptions['rabbitmq_uri'])) {
  $rabbitmq_uri = $hostoptions['rabbitmq_uri'];
  if (isset($hostoptions['rabbitmq_queue'])) {
    $rabbitmq_queue = $hostoptions['rabbitmq_queue'];
  } else {
    $rabbitmq_queue = "pecan";
  }

  # create the message
  $message = '{"folder": "' . $folder . '", "workflowid": "' . $workflowid . '"';
  if ($model_edit) {
    $message .= ', "modeledit": true';
  }
  $message .= '}';
  send_rabbitmq_message($message, $rabbitmq_uri, $rabbitmq_queue);

  #done
  $path = "05-running.php?workflowid=$workflowid&hostname={$hostname}";
  if ($pecan_edit) {
    $path .= "&pecan_edit=pecan_edit";
  }
  if ($model_edit) {
    $path .= "&model_edit=model_edit";
  }
  if ($offline) {
    $path .= "&offline=offline";
  }
  header("Location: {$path}");
} else {
  # start the actual workflow
  chdir($folder);

  if ($model_edit) {
    pclose(popen('R_LIBS_USER="' . $R_library_path . '" ' . $Rbinary . ' CMD BATCH --advanced  workflow.R &', 'r'));
  } else {
    pclose(popen('R_LIBS_USER="' . $R_library_path . '" ' . $Rbinary . ' CMD BATCH workflow.R &', 'r'));
  }

  #done
  $path = "05-running.php?workflowid=$workflowid&hostname={$hostname}";
  if ($pecan_edit) {
    $path .= "&pecan_edit=pecan_edit";
  }
  if ($model_edit) {
    $path .= "&model_edit=model_edit";
  }
  if ($offline) {
    $path .= "&offline=offline";
  }
  header("Location: {$path}");
}

close_database();
?>
