<?php
require("../common.php");

if (get_page_acccess_level() > 4) {
	header("Location: index.php");
}

# what is the current inputs id
if (isset($_REQUEST['id'])) {
	$id=$_REQUEST['id'];
} else {
	die('need an id');
}

$table = "inputs";

print_header("BETY", $table);
print_menu("BETY");

print_prev_next($id, $table);
print_editor($id, $table, true);

?>
	<hr />
	Existing Files<br/>
<?php
# get the input that we want to show
$query="SELECT dbfiles.id, concat(machines.hostname, ':', dbfiles.file_path, '/', dbfiles.file_name) as filename" .
       " FROM dbfiles, machines, inputs" .
       " WHERE inputs.id=$id AND dbfiles.container_id=inputs.file_id AND machines.id=dbfiles.machine_id;";
$result = mysql_query($query, $db_connection);
if (!$result) {
	die("Invalid query [$query] " . mysql_error($db_connection));
}
if (mysql_num_rows($result) > 0) {
?>
	<script type="text/javascript">
		function show_input(id) {
			var e = document.getElementById(id);
			var v = e.options[e.selectedIndex].value;
			window.location.href = "dbfiles_show.php?id=" + v;
		}
	</script>
	<div class="tbl" id="editor">
		<div class="row">
			<div class="key">
				<a href="javascript:void(0)" title="Show DBFile" onclick="show_input('file')">S</a>
			</div>
			<div class="val">
				<select id="file">
<?php
	while($dbfilerow = @mysql_fetch_assoc($result)) {
		print "<option value=\"{$dbfilerow['id']}\">{$dbfilerow['filename']}</option>\n";
	}
	mysql_free_result($result);
?>
				</select>
			</div>
		</div>
	</div>
<?php
}
print_footer();
?>
