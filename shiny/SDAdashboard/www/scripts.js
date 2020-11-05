$("[data-toggle='toggle']").click(function() {
  var selector = $(this).data("target");
  $(selector).toggleClass("in");
});
