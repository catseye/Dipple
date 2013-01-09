var w = 40;
var h = 40;

Actor = function() {
    this.init = function(x, y, color) {
        this.x = x;
        this.y = y;
        this.color = color;
        this.destX = undefined;
        this.destY = undefined;
        this.progress = undefined;
        this.state = 'rest';
        return this;
    };

    this.draw = function(ctx) {
        ctx.beginPath();
        ctx.fillStyle = this.color;
        var x = this.x * w + (w / 2);
        var y = this.y * h + (h / 2);
        if (this.progress !== undefined) {
            x += (this.progress / 100) * ((this.destX - this.x) * w);
            y += (this.progress / 100) * ((this.destY - this.y) * h);
        }
        ctx.arc(x, y, w/2, 0, 2 * Math.PI, false);
        ctx.fill();
    };

    this.setStateMoving = function(dx, dy) {
        if (this.state === 'rest') {
            this.destX = this.x + dx;
            this.destY = this.y + dy;
            this.progress = 0;
            this.state = 'moving';
        }
    };

    this.move = function() {
        if (this.state === 'moving') {
            this.progress += 10;
            if (this.progress >= 100) {
                this.x = this.destX;
                this.y = this.destY;
                this.destX = this.destY = this.progress = undefined;
                this.state = 'rest';
            }
        }
    };
};

SlideARound = function() {
    var actors;
    var canvas;
    var ctx;
    var intervalId;
    var counter = 0;

    this.draw = function() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        for (var i = 0; i < actors.length; i++) {
            actors[i].draw(ctx);
            actors[i].move();
        }
    }

    this.start = function(c) {
        actors = [];
        actors.push(new Actor().init(3, 4, "red"));
        actors.push(new Actor().init(2, 2, "blue"));

        canvas = c;
        ctx = canvas.getContext('2d');

        canvas.addEventListener('keydown', function(e) {
          switch (e.keyCode) {
            case 38:  /* Up arrow */
              actors[0].setStateMoving(0, -1);
              e.cancelBubble = true;
              break;
            case 40:  /* Down arrow */
              actors[0].setStateMoving(0, 1);
              e.cancelBubble = true;
              break;
            case 37:  /* Left arrow */
              actors[0].setStateMoving(-1, 0);
              e.cancelBubble = true;
              break;
            case 39:  /* Right arrow */
              actors[0].setStateMoving(1, 0);
              e.cancelBubble = true;
              break;
          }
        }, true);

        intervalId = setInterval(this.draw, 25);
    }
}
