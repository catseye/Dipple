var ctx;
var counter;
var interval_id;

function draw() {
  var canvas = document.getElementById("canvas");
  ctx = canvas.getContext('2d');

  $('#counter').html(counter);

  ctx.clearRect(0, 0, canvas.width, canvas.height);

  ctx.beginPath();
  ctx.lineWidth = "1";
  ctx.strokeStyle = "black";
  ctx.moveTo(0,0);
  var y = 200;
  var w = (canvas.width / counter);
  for (var i = 1; i <= counter; i++) {
      ctx.lineTo(w * i, y);
      y = (y == 200 ? 0 : 200);
  }
  ctx.lineTo(canvas.width,0);
  ctx.stroke();
  
  counter += 1;
}  

function start() {
  counter = 1;
  draw();
  interval_id = setInterval(draw, 50);
}

$(document).ready(function() {
  start();
});
