RetroBitMap = function() {
    var p;
    var canvas;
    var ctx;
    var intervalId;
    var scaleX = 2;
    var scaleY = 2;
  
    var img = new Image();

    var colors = [
      "#000000",
      "#0000ff",
      "#00ff00",
      "#00ffff",
      "#ff0000",
      "#ff00ff",
      "#ffff00",
      "#ffffff"
    ];

    var backingStore = new Array();
    var width = 40;
    var height = 25;
    this.color = 0;

    this.setColor = function(c) {
        this.color = c;
    };

    this.plot = function(x, y) {
        backingStore[x + y * width] = this.color;
    };

    this.drawFrame = function() {
        var status = document.getElementById('status');
        var start = new Date().getTime();
        var c;
        // trying to draw all 320x200 pixels per frame takes about
        // 360ms per frame
        /*
        for (var x = 0; x < width; x++) {
            for (var y = 0; y < height; y++) {
                ctx.fillStyle = colors[backingStore[x + y * width] || 0];
                ctx.fillRect(x * 2, y * 2, 2, 2);
            }
        }
        */
        for (var x = 0; x < width; x++) {
            for (var y = 0; y < height; y++) {
                ctx.fillStyle = colors[backingStore[x + y * width] || 0];
                ctx.fillRect(x * 16, y * 16, 16, 16);
            }
        }
        var middle = new Date().getTime();
        /*
        for (var i = 0; i < 1000; i++) {
            x = Math.floor(Math.random() * width);
            y = Math.floor(Math.random() * height);
            c = Math.floor(Math.random() * colors.length);
            this.setColor(c);
            this.plot(x, y);
        }
        */
        for (var x = 0; x < width; x++) {
            for (var y = 0; y < height; y++) {
                c = Math.floor(Math.random() * colors.length);
                this.setColor(c);
                this.plot(x, y);
            }
        }
        var lastly = new Date().getTime();
        status.innerHTML = 'draw: ' + (middle-start) + 'ms, update: ' + (lastly-middle) + 'ms';
    }

    this.start = function(c) {
        p = new Playfield();
        canvas = c;
        ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        var self = this;
        var fps = 60;
        canvas = c;
        var self = this;
        /*
        img.onload = function() {
            //self.draw();
            //interval_id = setInterval(self.draw, 20);
            intervalId = setInterval(function() { self.drawFrame(); }, 1000/fps);
        }
        img.src = 'charset.png';
        */
        intervalId = setInterval(function() { self.drawFrame(); }, 1000/fps);
    }
}
