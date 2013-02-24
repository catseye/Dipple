/*

60 fps is 16.666 msec per frame.
50 fps is 20 msec per frame.
30 fps is 33.333 msec per frame.

Trying to draw all 320x200 pixels per frame takes about 360ms per frame
(on Firefox 19 on Ubuntu 12.04, 2.1GHz x 2 laptop), at least by drawing
them each as a rectangle, so that's out.  But dividing the screen up into
"programmable" characters ought to be much faster.

Indeed, if they are just 8x8 pixels of a solid colour, the entire 40x25
screen can be updated in typ. less than 16 msec, meaning it can support
a frame rate of 60 fps.

If each also has a character blitted onto it, the screen can be updated
in typ. 26 msec, which is not quite inside 50 fps, but comfortably inside
30 fps.

Not scaling while blitting the character seem to make it slightly faster
(21 msec) but this is with the same 8x8 pixel source.  With a 16x16 pixel
source, it's comparable (23 msec?), so it's not all in the scaling.  (Also,
scaling anti-aliases the edges, which looks slightly un-retro.)

(Also these measurements are being taken at 5 fps, which seems to make
the update routine take more time, Omaha knows why.)

I'm sure we could add some (16?) larger (64x64?) sprites onto here with
few problems, especially if we're OK with 30fps.

To support 8 foreground colours, we copy the original charset bitmap
(black) into one of 8 hidden canvases, and use canvas pixel operations
to copy it into each of the other 7, using a different destination colour
in each.

*/

RetroBitMap = function() {
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

    var colorTriples = [
      [0, 0, 0],
      [0, 0, 255],
      [0, 255, 0],
      [0, 255, 255],
      [255, 0, 0],
      [255, 0, 255],
      [255, 255, 0],
      [255, 255, 255]
    ];

    var bgColorMemory = new Array();
    var fgColorMemory = new Array();
    var characterMemory = new Array();
    var chargen = new Array();
    var width = 40;
    var height = 25;
    this.bgColor = 0;
    this.fgColor = 0;

    this.setBgColor = function(c) {
        this.bgColor = c;
    };

    this.setFgColor = function(c) {
        this.fgColor = c;
    };

    this.plot = function(x, y, charnum) {
        fgColorMemory[x + y * width] = this.fgColor;
        bgColorMemory[x + y * width] = this.bgColor;
        characterMemory[x + y * width] = charnum;
    };

    this.drawFrame = function() {
        var status = document.getElementById('status');
        var start = new Date().getTime();
        var c;
        for (var x = 0; x < width; x++) {
            for (var y = 0; y < height; y++) {
                ctx.fillStyle = colors[bgColorMemory[x + y * width] || 0];
                ctx.fillRect(x * 16, y * 16, 16, 16);
                /* blit character image */
                var charNum = characterMemory[x + y * width] || 0;
                /* if img is charset8, use 8's in the source, but not the dest */
                ctx.drawImage(chargen[fgColorMemory[x + y * width] || 0],
                  /* source */ charNum * 16, 0, 16, 16,
                  /* dest   */ x * 16, y * 16, 16, 16);
            }
        }
        var middle = new Date().getTime();
        for (var x = 0; x < width; x++) {
            for (var y = 0; y < height; y++) {
                var bgColorNum = Math.floor(Math.random() * colors.length);
                var fgColorNum = Math.floor(Math.random() * colors.length);
                var charNum = Math.floor(Math.random() * 8);
                this.setBgColor(bgColorNum);
                this.setFgColor(fgColorNum);
                this.plot(x, y, charNum);
            }
        }
        var lastly = new Date().getTime();
        status.innerHTML = 'draw: ' + (middle-start) + 'ms, update: ' + (lastly-middle) + 'ms';
    };

    this.start = function(c) {
        canvas = c;
        ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        var self = this;
        var fps = 50;
        canvas = c;
        var self = this;
        img.onload = function() {
            self.createColoredCharsets();
            intervalId = setInterval(function() { self.drawFrame(); }, 1000/fps);
        }
        img.src = 'charset16.png';
    };

    this.createColoredCharsets = function() {
        var charset_0 = document.getElementById('charset_0');
        var charset_0_ctx = charset_0.getContext('2d');
        chargen[0] = charset_0;
        charset_0_ctx.drawImage(img, 0, 0, 128, 16);
        var imageData = charset_0_ctx.getImageData(0, 0, 128, 16);
        var w = imageData.width;
        var h = imageData.height;
        for (var color = 1; color < 8; color++) {
            var charset = document.getElementById('charset_' + color);
            chargen[color] = charset;
            var charset_ctx = charset.getContext('2d');
            var newData = charset_0_ctx.getImageData(0, 0, 128, 16);
            for (var y = 0; y < h; y++) {
                for (var x = 0; x < w; x++) {
                    var index = (y * w + x) * 4;
                    var red = imageData.data[index];
                    var green = imageData.data[index + 1];
                    var blue = imageData.data[index + 2];
                    var alpha = imageData.data[index + 3];
                    newData.data[index] = colorTriples[color][0];
                    newData.data[index + 1] = colorTriples[color][1];
                    newData.data[index + 2] = colorTriples[color][2];
                    newData.data[index + 3] = alpha;
                }
            }
            charset_ctx.putImageData(newData, 0, 0);
        }
    };
};
