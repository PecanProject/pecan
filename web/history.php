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
    header( "Location: index.php");
    close_database();
    exit;
  }
}
close_database();

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn History</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }

  function filter() {
    $(".unknown").toggle($("#unknown").is(':checked'));
    var query = "";
    if ($("#unknown").is(':checked')) {
      query += (query == "") ? "?" : "&";
      query += "show_unknown=on"
    }
    if ($("#onlyme").is(':checked')) {
      query += (query == "") ? "?" : "&";
      query += "onlyme=on"
    }
    if ($("#search").val() != "") {
      query += (query == "") ? "?" : "&";
      query += "search=" + $("#search").val()
    }

    // remote all elements
    $(".history").remove();
    $(".table").append('<div class="row history">Please wait loading history.</div>');
    $("#workflows").text("");

    // disable some things
    $("#unknown").attr("disabled", true);
    $("#onlyme").attr("disabled", true);
    $("#search").attr("disabled", true);

    // add new elements
    jQuery.get("historylist.php" + query, {}, function(data) {
      $(".history").remove();
      count=0;
      jQuery(data).find("workflow").each(function() {
        count++;
        var workflow = jQuery(this);
        var url = "";
        var style = "";
        if (workflow.attr("status") == "DONE") {
          style="background: #BBFFBB; color: black;";
          url="08-finished.php?workflowid=" + workflow.attr("id");
        } else if (workflow.attr("status") == "ERROR") {
          style="background: #FFBBBB; color: black;";
          url="08-finished.php?workflowid=" + workflow.attr("id");
        } else if (workflow.attr("status") == "RUNNING") {
          style="background: #BBFFFF; color: black;";
          url="05-running.php?workflowid=" + workflow.attr("id");
        } else {
          style="background: #FFFFFF; color: black;";
          url="";
        }

        row = '<div class="row history" style="' + style + '">';
        if (url != "") {
          row += '  <div class="cell"><a href="' + url + '">' + workflow.attr("id") + '</a></div>';
        } else {
          row += '  <div class="cell">' + workflow.attr("id") + '</div>';
        }
        row += '  <div class="cell">' + workflow.attr("sitename") + '</div>';
        row += '  <div class="cell">' + workflow.attr("modelname") + '</div>';
        row += '  <div class="cell">' + workflow.attr("name") + '</div>';
        row += '  <div class="cell">' + workflow.attr("start_date") + '</div>';
        row += '  <div class="cell">' + workflow.attr("end_date") + '</div>';
        row += '  <div class="cell">' + workflow.attr("started_at") + '</div>';
        row += '  <div class="cell">' + workflow.attr("finished_at") + '</div>';
<?php if (check_login() && (get_page_acccess_level() <= $min_delete_level)) { ?>
        row += '  <div class="cell"><a href="delete.php?workflowid=' + workflow.attr("id") + '">DELETE</a></div>';
<?php } ?>
        row += '</div>';
        $(".table").append(row);
      });
      $("#workflows").text(count + " workflows");

      // enable some things
      $("#unknown").attr("disabled", false);
      $("#onlyme").attr("disabled", false);
      $("#search").attr("disabled", false);
    });
  }

  $(document).ready(function () {
    filter();

    $("#search").keyup(function(event) {
      if (event.which == 13) {
        filter();
      }
    });
  });
</script>
</head>
<body>
  <div id="wrap">
    <div id="stylized">
      <h1>Legend</h1>
      <input type="text" readonly style="background: #BBFFBB; color: black;" value="Successful runs"/>
      <input type="text" readonly style="background: #FFBBBB; color: black;" value="Runs with errors"/>
      <input type="text" readonly style="background: #BBFFFF; color: black;" value="Ongoing runs"/>
      <input type="text" readonly style="background: #FFFFFF; color: black;" value="Runs in unknown state"/>
      <p></p>
      <label>Show runs in unknown state?</label>
      <input id="unknown" type="checkbox" onclick="filter();" checked="on"/>
      <label>Show only my runs?</label>
      <input id="onlyme" type="checkbox" onclick="filter();"/>
      <label>Filter history by text</label>
    	<input id="search" type="text"/>
      <p></p>
<?php if (!$authentication || (get_page_acccess_level() <= $min_run_level)) { ?>
      <form id="formprev" method="POST" action="01-introduction.php"> 
      <input id="prev" type="button" value="Start Over" onclick="prevStep();"/> 
      </form>
<?php } ?>
      <div class="spacer"></div>
<?php whoami(); ?>    
    </div>
    <div id="output">
      <h2>Execution Status <span id="workflows"></span></h2>
      <div class="table">
        <div class="row">
          <div class="header">ID</div>
          <div class="header">Site Name</div>
          <div class="header">Model Name</div>
          <div class="header">Model Type</div>
          <div class="header">Start Date</div>
          <div class="header">End Date</div>
          <div class="header">Started</div>
          <div class="header">Finished</div>
<?php if (check_login() && (get_page_acccess_level() <= $min_delete_level)) { ?>
          <div class="header">Delete</div>
<?php } ?>
        </div>
     </div>
    </div>
    <div id="footer"><?php echo get_footer(); ?></div>
  </div>
</body>  
