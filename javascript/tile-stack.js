// requires playfield.js
TileStack = function() {
    var p;
    var canvas;
    var ctx;
    var intervalId;
    var w = 40;
    var h = 40;
    var depth = 6;

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

        this.drawTile(3, 3, 0, "red");
        this.drawTile(4, 3, 0, "blue");
        this.drawTile(4, 3, 1, "red");
        this.drawTile(5, 3, 0, "yellow");
        this.drawTile(5, 3, 1, "red");
        this.drawTile(5, 3, 2, "blue");
        this.drawTile(5, 4, 0, "blue");
    };

    this.start = function(c) {
        p = new Playfield();
        canvas = c;
        ctx = canvas.getContext('2d');
        this.draw();
        var self = this;
        intervalId = setInterval(function() { self.draw(); }, 33);
    };
}
