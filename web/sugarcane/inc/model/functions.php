<?php
function sanitize($a){
    return preg_replace('/[\\<\\>\\"]/','',$a);
}
function strict_sanitize($a){
    return preg_replace('/[^a-zA-Z0-9\\-_\\.\\+\\=]/','',$a);
}
function line($line){
	return $line.PHP_EOL;
}
function explode_and_trim($separator,$string){
	$result = explode(",",$string);
	foreach($result as $k=>$v){
		$result[$k] = trim($v);
	}
	return $result;
}
function read_xml_structure($path){
	$structure_txt = file_get_contents($path, true);
	if($structure_txt==False) {
		echo "Error: $path is not readable.";
		exit();
	}
	$lines=explode("\n",$structure_txt);
	$i=0;
	foreach($lines as $line){
		if($line!="" and $line!=" "and $line[0]!='#'){
			if(preg_match_all('#\<[^,]+\>#', $line, $arr, PREG_PATTERN_ORDER)){
			}else{
				if(preg_match_all('#([^() ,<>]+)\(([^()]+)\)#',$line, $arr, PREG_SET_ORDER)){
					$items[$i][0]=trim($arr[0][1]);
					$items[$i][1]=explode_and_trim(',',$arr[0][2]);
					$i++;
				}
			}
		}
	}
	return $items;
}
function generate_xml($post,$path){
	$structure_txt = file_get_contents($path, true);
	if($structure_txt==False) {
		echo "$path is not readable. Try changing the permission of the destination folder and $path to 777 if it exists.";
		exit();
	}
	$lines=explode("\n",$structure_txt);
	$i=0;
	$return_value="";
	foreach($lines as $line){
		if($line!="" and $line!=" "and $line[0]!='#'){
			if(preg_match_all('#\<[^,]+\>#', $line, $arr, PREG_PATTERN_ORDER)){
				$return_value.=line(trim($arr[0][0]));
			}else{
				if(preg_match_all('#([^(), <>]+)\(([^()]+)\)#',$line, $arr, PREG_SET_ORDER)){
					$tag=sanitize(trim($arr[0][1]));
					$items=explode_and_trim(',',$arr[0][2]);
					$i++;
					$content=line("<$tag>");
					$show=false;
					foreach ($items as $var) {
						$var=sanitize($var);
						$var_name=$tag."_".$var;
						$var_name=preg_replace('/\./','_',$var_name);
						if(isset($post[$var_name]) and $post[$var_name]!=""){
							$inside=sanitize($post[$var_name]);
							$content.=line("<$var>$inside</$var>");
							$show=true;
						}
					}
					$content .= line("</$tag>");

					if($show==true){
						$return_value.= $content;
					}

				}
			}
		}
	}
	return $return_value;
}
function get_default($path){
	$default_xml_raw = file_get_contents($path, true);
	$return="";
	if($default_xml_raw==False) {
		echo "$path is not readable. Try changing the permission of the destination folder and $path to 777 if it exists.";
		exit();
	}
	if(preg_match_all('#<([^<>/]*)>[ \n\r]*([^< >\n\r]+)[ \n\r]*</\1>#', $default_xml_raw, $arr, PREG_PATTERN_ORDER)){
		foreach($arr[1] as $index=>$tag){
			$value=trim($arr[2][$index]);
			#print($value . "<br />");
			if(preg_match('#<([^</!?\">]+)>[ \r\n]*(?:<([^<>]+)>.*</\2>[ \r\n]*)*[ \r\n]*<'.$tag.'>[ \r\n]*'.$value.'#', $default_xml_raw, $arr2)){
				$parent_tag=trim($arr2[1]);
				$var_name=$parent_tag.'_'.$tag;
				$return.= "$('[name=\"$var_name\"]').val('$value'); ";
			}
		}

	}
	return $return;
}
?>
