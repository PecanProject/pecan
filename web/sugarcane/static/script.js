$(document).ready(function () {
    $('#myTab a:first').tab('show');

    $('a').click(function (e) {
        e.preventDefault();
        $(this).tab('show');
        this.blur();
    });

});
