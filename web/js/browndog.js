//Brown Dog graph
function browndog_add() {
  var node = document.getElementById('browndog_img');
  if (node) return;
  
  var graphic = $('<img>')
    .attr('src', 'images/browndog-small-transparent.gif')
    .attr('width', '25')
    .attr('id', 'browndog_img')
    .css('position', 'absolute')
    .css('left', '0px')
    .css('bottom', '45px');
  $("body").append(graphic);

  setTimeout(browndog_run, 10);
}

function browndog_del() {
  var node = document.getElementById('browndog_img');
  node.parentNode.removeChild(node);
  
  node = document.getElementById('browndog_poweredby');
  node.parentNode.removeChild(node);
}

function browndog_run() {
  var graphic = document.getElementById('browndog_img');
  graphic.style.left = parseInt(graphic.style.left) + 25 + 'px';

  if(parseInt(graphic.style.left) < $(window).width() - 50) {
    setTimeout(browndog_run, 10);
  } else {
    //graphic.remove();
    graphic.parentNode.removeChild(graphic);

    //Add powered by graphic
    graphic = $('<img>')
      .attr('src', 'images/poweredby-transparent.gif')
      .attr('id', 'browndog_img')
      .attr('width', '100');

    var link = $('<a/>')
      .attr('href', 'http://browndog.ncsa.illinois.edu')
      .attr('id', 'browndog_poweredby')
      .css('position', 'fixed')
      .css('right', '10px')
      .css('bottom', '30px')
      .append(graphic);

    $("body").append(link);
  }
}
