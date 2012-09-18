var ctx;
var p;
var interval_id;
var w = 40;
var h = 40;

var colors = ["red", "black", "green", "blue"];

function draw() {
    var canvas = document.getElementById("canvas");
    ctx = canvas.getContext('2d');

    ctx.clearRect(0, 0, canvas.width, canvas.height);

    var c = colors[Math.floor(Math.random() * 4)]
    var x = Math.floor(Math.random() * 10)
    var y = Math.floor(Math.random() * 10)
    
    if (Math.random() > 0.25) {
        p.put(x, y, c);
    } else {
        p.put(x, y, undefined);
    }

    p.foreach(function (x, y, value) {
        ctx.beginPath();
        ctx.fillStyle = value;
        ctx.arc(x * w + (w / 2), y * h + (h / 2), w/2, 0, 2 * Math.PI, false);
        ctx.fill();
    });
}  

function start() {
    p = Playfield();
    draw();
    interval_id = setInterval(draw, 25);
}

$(document).ready(function() {
    start();
});
