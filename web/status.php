<html>
    <head>
    <link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
    <meta http-equiv="refresh" content="60">
    </head>
    <body>
<?php

  // execute R script from shell
  // this will save a plot at temp.png to the filesystem
  exec("Rscript PEcAn.Network.R");
 
  // return image tag
  echo("<img src='NetworkStatus.png' />");

?>

</body>
</html>