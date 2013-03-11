// requires playfield.js
TileStack = function() {
    var p;
    var canvas;
    var ctx;
    var intervalId;
    var w = 40;
    var h = 40;
    var depth = 6;
    var timer = 0;
    var selTileX = undefined;
    var selTileY = undefined;

    var colors = ["red", "yellow", "green", "blue"];

    this.drawTile = function(tileX, tileY, tileZ, color) {
        ctx.beginPath();
        var xLeft = tileX * w - (depth * tileZ);
        var yTop = tileY * h - (depth * tileZ);
        ctx.beginPath();
        ctx.fillStyle = color;
        ctx.lineWidth = 1;
        ctx.strokeStyle = "black";
        ctx.moveTo(xLeft, yTop);
        ctx.lineTo(xLeft + w, yTop);
        ctx.lineTo(xLeft + w, yTop + h);
        ctx.lineTo(xLeft, yTop + h);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(xLeft + w, yTop);
        ctx.lineTo(xLeft + w + depth, yTop + depth);
        ctx.lineTo(xLeft + w + depth, yTop + h + depth);
        ctx.lineTo(xLeft + w, yTop + h);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(xLeft, yTop + h);
        ctx.lineTo(xLeft + depth, yTop + h + depth);
        ctx.lineTo(xLeft + w + depth, yTop + h + depth);
        ctx.lineTo(xLeft + w, yTop + h);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
    };

    this.draw = function() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        var c = colors[Math.floor(Math.random() * colors.length)];
        var x = Math.floor(Math.random() * 13) + 1;
        var y = Math.floor(Math.random() * 8) + 1;

        if (timer % 10 === 0) {
            if (Math.random() > 0.25) {
                var stack = p.get(x, y);
                if (stack === undefined) {
                    stack = [];
                    p.put(x, y, stack);
                }
                stack.push(c);
                //p.put(x, y, [c]);
            } else {
                var stack = p.get(x, y);
                if (stack !== undefined) {
                    stack.pop();
                }
            }
        }
        timer++;

        var self = this;
        p.foreach(function (x, y, value) {
            for (var i = 0; i < value.length; i++) {
                var c = value[i];
                if (selTileX === x && selTileY === y) {
                    c = "purple";
                }
                self.drawTile(x, y, i, c);
            }
        });

    };

    this.start = function(c) {
        p = new Playfield();
        canvas = c;
        ctx = canvas.getContext('2d');

        canvas.onmousedown = function(e) {
          var can_x = e.pageX - canvas.offsetLeft;
          var can_y = e.pageY - canvas.offsetTop;
          
          selTileX = Math.floor(can_x / w);
          selTileY = Math.floor(can_y / h);
          self.drawTile(tileX, tileY, 0, "purple");
        }
        canvas.onmouseup = function() {
          selTileX = selTileY = undefined;
        };

        var self = this;
        intervalId = setInterval(function() { self.draw(); }, 33);
    };
}
