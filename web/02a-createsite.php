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
  if (get_page_acccess_level() > $min_run_level) {
    header( "Location: history.php");
    close_database();
    exit;
  }
}

# parameters
if (!isset($_REQUEST['lat'])) {
  die("Need latitude.");
}
$lat=$_REQUEST['lat'];
if (!isset($_REQUEST['lon'])) {
  die("Need longitude.");
}
$lon=$_REQUEST['lon'];

// get sitegroups
$sitegroupid=$_REQUEST['sitegroupid'] ?: "-1";
$query = "SELECT id, name FROM sitegroups WHERE public_access OR user_id=? ORDER BY name";
$stmt = $pdo->prepare($query);
if (!$stmt->execute(array(get_userid()))) {
  die('Invalid query: ' . error_database());
}
$sitegroups = "";
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
  if ($sitegroupid == $row['id']) {
    $sitegroups .= "<option value='{$row['id']}' selected>{$row['name']}</option>\n";
  } else {
    $sitegroups .= "<option value='{$row['id']}'>{$row['name']}</option>\n";    
  }
}
if ($sitegroupid == "-1") {
  $sitegroups .= "<option value='-1' selected>All Sites</option>\n";
} else {
  $sitegroups .= "<option value='-1'>All Sites</option>\n";    
}

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Create Site</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css?id=<?php echo rand(); ?>" />
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
<script type="text/javascript">
  function validate() {
    $("#error").html("&nbsp;");
    if ($("#newsite").val() == "") {
      $("#error").html("Enter sitename to continue");
      return false;
    }
    // if ($("#city").val() == "") {
    //   $("#error").html("Enter city to continue");
    //   return false;
    // }
    // if ($("#state").val() == "") {
    //   $("#error").html("Enter state to continue");
    //   return false;
    // }
    // if ($("#country").val() == "") {
    //   $("#error").html("Enter country to continue");
    //   return false;
    // }
    // if ($("#timezone").val() == "") {
    //   $("#error").html("Enter timezone to continue");
    //   return false;
    // }
    if ($("#lat").val() == "") {
      $("#error").html("Enter lat to continue");
      return false;
    }
    if ($("#lon").val() == "") {
      $("#error").html("Enter lon to continue");
      return false;
    }
    // if ($("#elevation").val() == "") {
    //   $("#error").html("Enter elevation to continue");
    //   return false;
    // }
    // if ($("#temperature").val() == "") {
    //   $("#error").html("Enter temperature to continue");
    //   return false;
    // }
    // if ($("#percipitation").val() == "") {
    //   $("#error").html("Enter percipitation to continue");
    //   return false;
    // }
    // if ($("#clay").val() == "") {
    //   $("#error").html("Enter clay to continue");
    //   return false;
    // }
    // if ($("#sand").val() == "") {
    //   $("#error").html("Enter sand to continue");
    //   return false;
    // }
    // if ($("#soilnotes").val() == "") {
    //   $("#error").html("Enter soilnotes to continue");
    //   return false;
    // }
    // if ($("#notes").val() == "") {
    //   $("#error").html("Enter notes to continue");
    //   return false;
    // }
    return true;
  }
      
  function prevStep() {
    $("#formprev").submit();
  }

  function createSite() {
    if (!validate()) return;
    var serializedData = $('#site :input').serialize();
    $.post("insert-site.php", serializedData, function(data) {
      var result = data.split(" ");
      $("#siteid").val(result[0]);
      $("#sitename").val(result[1]);
      $("#sitegroupid").val(result[2]);
      $("#formprev").submit();
    }).fail(function(data) {
      $("#error").html("Could not submit site : " + data.statusText);
      console.log(data);
    })
    event.preventDefault();
  }

  $(document).ready(function () {
    validate();
  });
</script>
</head>
<body>
<div id="wrap">
  <div id="stylized">
    <h1>Create new site</h1>
    <p>All fields in the form are required.</p>

    <form id="formprev" method="POST" action="02-modelsite.php">
<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"{$key}[]\" id=\"{$key}[]\" type=\"hidden\" value=\"{$v}\"/>";
      }
    } else {
      if(strcmp($key, "notes") == 0) {
        $str = htmlentities($value, ENT_QUOTES);
        echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$str}\"/>";
      } else {
        echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$value}\"/>";
      }
    }
  }
?>
    </form>

      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Back" onclick="prevStep();" />
      <input id="next" type="button" value="Add Site" onclick="createSite();" />
      <div class="spacer"></div>
    </form>
<?php left_footer(); ?>    
  </div>
  <div id="output">
    <p>This form allows you to add a new site. Only those fields marked with <b>*</b> are required.</p>
    <form id="site">
      <fieldset>
        <legend>Site Information</legend>

        <label>Site name (*required):</label>
        <input id="newsite" size="30" type="text" name="newsite"></input>
        <div class="spacer"></div>

        <label>Site Group:</label>
        <select name="newsitegroupid" id="newsitegroupid">
          <?php echo $sitegroups; ?>
        </select>
        <div class="spacer"></div>

        <label>City:</label>
        <input id="city" size="30" type="text" name="city"></input>
        <div class="spacer"></div>

        <label>State:</label>
        <input id="state" size="30" type="text" name="state"></input>
        <div class="spacer"></div>

        <label>Country:</label>
        <input id="country" size="30" type="text" name="country"></input>
        <div class="spacer"></div>

        <label>Timezone:</label>
        <input id="timezone" size="30" type="text" name="timezone"></input>
        <div class="spacer"></div>

        <label>Latitude (*required):</label>
        <input id="lat" size="30" type="text" name="lat" value="<?php echo $lat; ?>"></input>
        <div class="spacer"></div>

        <label>Longitude (*required):</label>
        <input id="lon" size="30" type="text" name="lon" value="<?php echo $lon; ?>"></input>
        <div class="spacer"></div>

        <label>Elevation (m):</label>
        <input id="elevation" size="30" type="text" name="elevation"></input>
        <div class="spacer"></div>
      </fieldset>

      <fieldset>
        <legend>Site Meteorological Information</legend>

        <label>Mean Annual Precipitation (mm/yr):</label>
        <input id="percipitation" size="30" type="text" name="percipitation"></input>
        <div class="spacer"></div>

        <label>Mean Annual Temperature (C):</label>
        <input id="temperature" size="30" type="text" name="temperature"></input>
        <div class="spacer"></div>
      </fieldset>

      <fieldset>
        <legend>Site Soil Information</legend>

        <label>% Clay:</label>
        <input id="clay" size="30" type="text" name="clay"></input>
        <div class="spacer"></div>

        <label>% Sand:</label>
        <input id="sand" size="30" type="text" name="sand"></input>
        <div class="spacer"></div>

        <label>Soil Notes:</label>
        <textarea id="soilnotes" cols="40" rows="5" type="text" name="soilnotes"></textarea>
        <div class="spacer"></div>
      </fieldset>

      <fieldset>
        <legend>Site Notes</legend>

        <label>Site Notes:</label>
        <textarea id="notes" cols="40" rows="5" type="text" name="notes"></textarea>
        <div class="spacer"></div>
      </fieldset>

      <div class="spacer"></div>
      <input id="create" type="button" style="width: 100%" value="Add Site" onclick="createSite();" />
      <div class="spacer"></div>
    </form>
  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
close_database();
?>
