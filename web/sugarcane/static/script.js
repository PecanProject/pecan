$(document).ready(function () {
    $('#myTab a:first').tab('show');

    $('a').click(function (e) {
        e.preventDefault();
        $(this).tab('show');
        this.blur();
    });

    $("#create_xml").click(function(event){
        event.preventDefault();
        $.post($("#form").attr("action"), $("#form").serialize()+"&command=create_xml",
            function(data) {
                if(data=="successful"){
                    $("#feedback").html("successful");
                    $("#feedback").stop().fadeTo(70,1).delay(800).fadeTo(500,0); 
                }else{
                    alert(data);
                }
            });
        this.blur();
    });
    $("#run_btn").click(function(event){
        event.preventDefault();
        var run_btn_text=$("#run_btn").html();
        var page_title=document.title;
        var count=1;
        var timer=setInterval(function(){run_timer()},1000);

        function run_timer(){
            document.title="["+count+"] Running - "+page_title;
            count++;
        }


        $("#run_btn").html("Running...");
        $("#run_btn").addClass("disabled");
        $.post("#", { command: "run"},
            function(data) {
                if(data=="successful"){
                    clearInterval(timer);
                    document.title=page_title;
                    $("#run_btn").html(run_btn_text);
                    $("#run_btn").removeClass("disabled");
                    $("#feedback").html("Running successful.");
                    $("#feedback").stop().fadeTo(70,1).delay(800).fadeTo(500,0); 
                }else{
                    alert(data);
                }
            }
            );
        this.blur();
    });

    $('a[href="#graph"]').hide();
    $("button#plot_btn").click(function(event){
        event.preventDefault();
        $("#graph_box").fadeTo(0,0);
        $('a[href="#graph"]').tab('show');
        $('a[href="#graph"]').show();
        $(".loading").show();
        $.post($("#form").attr("action"), $("#form").serialize()+"&command=plot",
            function(data) {
                if(data.indexOf(".png")>-1){
                    $("#graph_box").html("<img src='"+data+"' />");
                    $(".loading").hide();
                    $("#graph_box").fadeTo(600,1);
                }	
            }
            );
        this.blur();
    });
    $("#default").click(function(event){
        event.preventDefault();
        $.getScript("?command=default");
        this.blur();
    });
    $("#clear").click(function(event){
        event.preventDefault();
        $("input").val("");
        this.blur();
    });
    $("#graph_settings_btn").click(function(event){
        event.preventDefault();
        $('a[href="#graph_settings"]').tab('show');
        this.blur();
    });

    var sa_btn_text=$("#select_area_btn").html();
    var select_area_enabled=false;
    function toggle_area_selection(){
        $("#select_area_btn").button('toggle');
        if(select_area_enabled==true){
            $("#select_area_btn").html(sa_btn_text);
            select_area_enabled=false;
        }else{
            $("#select_area_btn").html("Drag on the map to select");
            select_area_enabled=true;
        }
    }
    $("#select_area_btn").click(function(event){
        event.preventDefault();
        toggle_area_selection();
        this.blur();
    });

    function validate_radio_buttons(){
        var names = [];
        var valid = true;
        $('#graph_settings input[type="radio"]').each(function() {
            names[$(this).attr('name')] = true;
        });
        for (name in names) {
            var radio_buttons = $("input[name='" + name + "']");
            if (radio_buttons.filter(':checked').length == 0) {
                valid=false;
            } 
        }
        if(valid){
            return true;
        }else{
            return false;
        }
    }
    function graph_settings_btn_check(){
        if(validate_radio_buttons()){
            // all variables have been selected
            if($("button#plot_btn").attr("disabled")=="disabled"){
                $("button#plot_btn").removeAttr("disabled");
            }
        }else{
            $("button#plot_btn").attr("disabled","disabled");
        }
    }
    graph_settings_btn_check();
    $('#graph_settings input:radio').change(function() {
        graph_settings_btn_check();
    });

    var select_area_enabled=false;
    function initialize_map() {
        var myOptions = {
            center: new google.maps.LatLng(27.13823, -81.15023),
            zoom: 13,
            mapTypeId: google.maps.MapTypeId.HYBRID,
            streetViewControl: false,
            zoomControl: true,
            zoomControlOptions : {
                style: google.maps.ZoomControlStyle.SMALL
            },
            /*mapTypeControlOptions: {
                mapTypeIds: ['One','Two'],
                style: google.maps.MapTypeControlStyle.DROPDOWN_MENU
            },*/
            maxZoom: 16,
            minZoom: 2
        };



        var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
        google.maps.event.trigger(map, 'resize');

        var ne_lat=0,ne_lng=0,sw_lat=0,sw_lng=0;
        var latlng1,latlng2;


        function updateInfobar1(){
            var latlong=map.getCenter();
            $("#infobox1").html("</br>Center</br>Latitute: <b>"+latlong.lat().toFixed(5)+"</b></br>Longitude: <b>"+latlong.lng().toFixed(5)+"</b>"
                    +"</br></br><b>Selected Area</b></br>Top Right</br>Latitute: <b>"+ne_lat.toFixed(5)+"</b></br>Longitude: <b>"+ne_lng.toFixed(5)+"</b>"
                    +"</br>Bottom Left</br>Latitute: <b>"+sw_lat.toFixed(5)+"</b></br>Longitude: <b>"+sw_lng.toFixed(5)+"</b>"
                    +"</br>Bottom Right</br>Latitute: <b>"+sw_lat.toFixed(5)+"</b></br>Longitude: <b>"+ne_lng.toFixed(5)+"</b>"
                    +"</br>Top Left</br>Latitute: <b>"+ne_lat.toFixed(5)+"</b></br>Longitude: <b>"+sw_lng.toFixed(5)+"</b>"
                    );
        }
        function updateLocation(latlong){
            updateInfobar1();
        }
        updateLocation(map.getCenter());

        google.maps.event.addListener(map, 'center_changed', function() {
            updateLocation(map.getCenter());
        });

        
        var rect;
        function init_rect(){
            rect=new google.maps.Rectangle({
                strokeColor: "#FF0000",
                strokeOpacity: 0.8,
                strokeWeight: 0,
                fillColor: "#FF0000",
                fillOpacity: 0.20,
                map: map
            }); 
        }
        init_rect();
        var dragging;
        function update_area_info(){
            ne_lat=rect.getBounds().getNorthEast().lat();
            ne_lng=rect.getBounds().getNorthEast().lng();
            sw_lat=rect.getBounds().getSouthWest().lat();
            sw_lng=rect.getBounds().getSouthWest().lng();
            if(ne_lat<sw_lat){
                var temp=ne_lat;
                ne_lat=sw_lat;
                sw_lat=temp;
            }
            if(ne_lng<sw_lng){
                var temp=ne_lng;
                ne_lng=sw_lng;
                sw_lng=temp;
            }
            toggle_area_selection();
            updateInfobar1();
        }
        var listen2, listen3, listen4, listen5, listen6;
        function selection_listers_on(){
            listen2 = google.maps.event.addListener(map, 'mousemove', function(mEvent) {
                if(!select_area_enabled) return;
                updateRect(mEvent);
            }); 
            listen3 = google.maps.event.addListener(rect, 'mousemove', function(mEvent) {
                if(!select_area_enabled) return;
                updateRect(mEvent);
            }); 
            listen4 = google.maps.event.addListener(map, 'mouseup', function(mEvent) {
                if(!select_area_enabled) return;
                map.draggable = true;
                dragging = false;

                update_area_info();
                if(!select_area_enabled) selection_listers_off();
            }); 
            listen6 = google.maps.event.addListener(rect, 'mouseup', function(data){
                if(!select_area_enabled) return;
                map.draggable = true;
                dragging = false;

                update_area_info();
                if(!select_area_enabled) selection_listers_off();
            }); 
        }
        function selection_listers_off(){
            if(listen2) google.maps.event.removeListener(listen2);
            if(listen3) google.maps.event.removeListener(listen3);
            if(listen4) google.maps.event.removeListener(listen4);
            if(listen6) google.maps.event.removeListener(listen6);
        }

        var listen1 = google.maps.event.addListener(map, 'mousedown', function(mEvent) {
            if(!select_area_enabled) return;
            map.draggable = false;
            latlng1 = mEvent.latLng;
            dragging = true;
            if(select_area_enabled) selection_listers_on();
        }); 
        listen5 = google.maps.event.addListener(rect, 'mousedown', function(mEvent){
            if(!select_area_enabled) return;
            map.draggable = false;
            latlng1 = mEvent.latLng;
            dragging = true;
            if(select_area_enabled) selection_listers_on();
        }); 

        function updateRect(mEvent) {
            latlng2=mEvent.latLng;
            if(dragging){
                cursorCoord = mEvent.latLng;
                if(cursorCoord.lng()<latlng1.lng()){
                    var latLngBounds = new google.maps.LatLngBounds(latlng2, latlng1);
                }else{
                    var latLngBounds = new google.maps.LatLngBounds(latlng1, latlng2);
                }
                rect.setBounds(latLngBounds);
            }   
        }   

    }


    var initialized=false;
    $("#maps-tab-button").click(function(event){
        event.preventDefault();
        if(initialized==false){
            initialize_map();
            initialized=true;
        }
        this.blur();
    });
    $('.noselect').live('selectstart dragstart', function(evt){ evt.preventDefault(); return false; });

});
