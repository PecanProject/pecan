<!DOCTYPE html>
<html lang="en">
<head> 
	<meta charset="UTF-8" /> 
	<title>Sugarcane</title>
	<link rel="stylesheet" href="static/bootstrap.min.css" type="text/css" />
	<link rel="stylesheet" href="static/bootstrap-responsive.min.css" type="text/css" />
	<link rel="stylesheet" href="static/style.css" type="text/css" />
	<script type="text/javascript" src="static/jquery-1.7.2.min.js"></script>
	<script type="text/javascript" src="static/bootstrap.min.js"></script>
	<script type="text/javascript" src="static/bootstrap-tab.js"></script>
	<script type="text/javascript" src="static/bootstrap-button.js"></script>
	<script type="text/javascript" src="static/script.js"></script>
        <script type="text/javascript" src="http://maps.googleapis.com/maps/api/js?key=AIzaSyBUevuOVPkSqy35NkSqYIQ6UeqsrzYqS54&sensor=true"> </script>
</head>
<body>
	<div id="zcontainer">
		<div class="contain-left">
			<form name="input" id="form" action="#" method="post">
				<div class="tabbable tabs-left">
					<ul class="nav nav-tabs fixed">
						<? foreach ($items as $key=>$item) { ?>
						<li<? if($key==0) echo ' class="active"';?>><a href="#<?=$item[0]?>"><?=$item[0]?></a></li>
						<? } ?>
                                                <li><a href="#graph_settings" style="margin-top:20px;">Settings</a></li>
                                                <li><a href="#graph">Graph</a></li>
                                                <li><a href="#maps-tab" id="maps-tab-button" style="margin-top:20px;">Map</a></li>
						<div class="buttons">
							<button class="btn" type="submit" id="default">Load Default</button><br />
							<button class="btn" type="submit" id="clear">Clear</button><br />
							<button class="btn" type="submit" id="create_xml">Generate XML</button><br />
							<button class="btn" type="submit" id="run_btn">Run</button><br />
							<button class="btn" type="submit" id="graph_settings_btn">Graph Settings</button><br />
							<button class="btn" type="submit" id="plot_btn" disabled="diabaled">Plot</button><br />
							<div id="feedback"></div>
						</div>
					</ul>

					<div class="tab-content">
						<? foreach ($items as $key=>$item) { ?>
						<div class="tab-pane<? if($key==0) echo " active";?>" id="<?=$item[0]?>">
							<div class="form-horizontal">
								<fieldset>
									<legend><?=$item[0]?></legend>
									<? foreach ($item[1] as $var) { 
									$var_name=$item[0].'_'.$var; ?>
									<div class="control-group">
										<label class="control-label" for="<?=$var_name?>"><?=$var?></label>
										<div class="controls">
											<input type="text" name="<?=$var_name?>" />
										</div>
									</div>
									<? } ?>
								</fieldset>
							</div>
						</div>
						<? } ?>

						<div class="tab-pane" id="graph_settings">
                                                    <div class="plot_settings">
                                                        <h2>Graph Setings</h2>
                                                        <table class="table table-striped">
                                                        <thead>
                                                        <tr>
                                                        <th>Variables</th>
                                                        <th>x-axis</th>
                                                        <th>y-axis</th>
                                                        </tr>
                                                        </thead>
                                                        <tbody>
                                                        <tr>

                                                        <? foreach($graph_variables as $gvar){ ?>
                                                        <td><?=$gvar;?></td>
                                                        <td><input type="radio" name="var_group_x" value="<?=trim($gvar);?>"></td>
                                                        <td><input type="radio" name="var_group_y" value="<?=trim($gvar);?>"></td>
                                                        </tr>
                                                        <tr>
                                                        <? } ?>
                                                        </tbody>
                                                        </table>
                                                    </div>
						</div>
                                                <div class="tab-pane" id="graph">
                                                    <div class="loading" style="display:none"><img src="static/ajax-loader.gif" /></div>
                                                    <div id="graph_box"></div>
                                                </div>
                                                <div class="tab-pane" id="maps-tab">
                                                    <div id="map_canvas" class="noselect"></div>
                                                    <div id="map_control"><button id="select_area_btn" class="btn" data-toggle="button" >Select Area</button><div id="infobox1"></div></div>
                                                </div>
					</div>
				</div>
			</form>
		</div>
	</div>
</body>
</html>
