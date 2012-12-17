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
</head>
<body>
	<div id="zcontainer">
		<div class="contain-left">
			<h3 style="padding-bottom:10px;">Set BioCro Parameters</h3>
			<form name="input" id="form" action="#" method="post">
				<div class="tabbable tabs-left">

					<ul class="nav nav-tabs fixed">
						<? foreach ($tabs as $key=>$tab) { ?>
						<li<? if($key==0) echo ' class="active"';?>><a href="#<?=$tab[0]?>"><?=$tab[0]?></a></li>
						<? } ?>
						<div class="buttons">
							<input type="hidden" name="command" value="continue"/>
							<button class="btn" type="submit" id="create_xml">Continue</button><br />
						</div>
					</ul>

					<div class="tab-content">
						<? foreach ($tabs as $key=>$tab) { ?>
						<div class="tab-pane<? if($key==0) echo " active";?>" id="<?=$tab[0]?>">
							<div class="form-horizontal">
								<fieldset>
									<legend><?=$tab[0]?></legend>
									<? foreach ($tab[1] as $param) { 
									$param_name=$tab[0].'_'.$param->nodeName; ?>
									<div class="control-group">
										<label class="control-label" for="<?=$param_name?>"><?=$param->nodeName?></label>
										<div class="controls">
											<input type="text" name="<?=$param->getNodePath()?>" value="<?=$param->nodeValue?>"/>
										</div>
									</div>
									<? } ?>
								</fieldset>
							</div>
						</div>
						<? } ?>
					</div>

				</div>
			</form>
		</div>
	</div>
</body>
</html>
