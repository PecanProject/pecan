<?php
/**
 * Copyright (c) 2017 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// This page is designed to act as the template page for all the configurations setups
// This page only have footer part.

?>
    </div>
    <div id="footer"><?php include "../common.php"; echo get_footer(); ?></div>
</div>
<!--bootstrap javascripts -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.3/jquery.min.js" integrity="sha384-I6F5OKECLVtK/BL+8iSLDEHowSAfUo76ZL9+kGAgTRdiByINKJaqTPH/QVNS1VDb" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.6/js/bootstrap.min.js" integrity="sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS" crossorigin="anonymous"></script>
<script src="https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js"></script>
<!--custom javascripts-->
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
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
</body>
</html>
