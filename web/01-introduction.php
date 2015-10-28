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

if ($authentication) {
    open_database();
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
    close_database();
}

?>
<!DOCTYPE html>
<html>
<head>
<title>EBI Sites</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
    function validate() {
        $("#error").html("");
    }
        
    function prevStep() {
        $("#formprev").submit();
        }

    function nextStep() {
        console.log($("#formnext"));
        $("#formnext").submit();
    }
</script>
</head>
<body>
<div id="wrap">
    <div id="stylized">
        <form id="formprev" method="POST" action="history.php">
        </form>
        <form id="formnext" method="POST" action="02-modelsite.php">
            <h1>Introduction</h1>
            <p>Below you will find the buttons to step through the
            workflow creation process.</p>

            <label>Offline mode:</label>
            <span id="error" class="small">This will disable Google Maps</span>
            <input name="offline" id="offline" style="align: left" type="checkbox" value="offline">
            <div class="spacer"></div>

            <p></p>
            <label>Workflow</label>
            <span id="error" class="small">&nbsp;</span>
            <input id="prev" type="button" value="History" onclick="prevStep();" />
            <input id="next" type="button" value="Next" onclick="nextStep();" />
            
            <div class="spacer"></div>
        </form>
<?php
  if (check_login()) {
    echo "<p></p>";
    echo "Logged in as " . get_user_name();
    echo "<a href=\"index.php?logout\" id=\"logout\">logout</a>";
  }
?>    
    </div>
    <div id="output">
        <h1>Introduction</h1>
        <p>The following pages will guide you through setting up a
        PEcAn worklflow. You will be able to always go back to a
        previous step to change inputs. However once the model is
        running it will continue to run until it finishes. You will
        be able to use the history button to jump to existing 
        executions of PEcAn.</p>
        <p>The following webpages will help to setup the PEcAn
        workflow. You will be asked the following questions:</p>
        <ol>
        <li><b>Host and Model</b> You will first select the host to
        run the workflow on as well as the model to be exectuted.</li>
        <li><b>Site</b> The next step is to select the site where
        the model should be run for.</li>
        <li><b>Model Parameters</b> Based on the site some final
        parameters for the model will need to be selected.</li>
        <li><b>Model Execution</b> Once all variables are selected
        PEcAn will execute the workflow.</li>
        <li><b>Results</b> After execution of the PEcAn workflow you
        will be presented with a page showing the results of the
        PEcAn workflow.</li> 
        </ol>
    </div>
    <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>
