Playfield = function() {
    this._store = {};
    this.min_x = undefined;
    this.min_y = undefined;
    this.max_x = undefined;
    this.max_y = undefined;

    this.get = function(x, y) {
        return this._store[x+','+y];
    };

    this.put = function(x, y, value) {
        if (this.min_x === undefined || x < this.min_x) this.min_x = x;
        if (this.max_x === undefined || x > this.max_x) this.max_x = x;
        if (this.min_y === undefined || y < this.min_y) this.min_y = y;
        if (this.max_y === undefined || y > this.max_y) this.max_y = y;
        if (value === undefined) {
            delete this._store[x+','+y];
        }
        this._store[x+','+y] = value;
    };
          
    this.foreach = function(fun) {
        for (var y = this.min_y; y <= this.max_y; y++) {
            for (var x = this.min_x; x <= this.max_x; x++) {
                var key = x+','+y;
                var value = this._store[key];
                if (value === undefined)
                    continue;
                var result = fun(x, y, value);
                if (result !== undefined) {
                    if (result === ' ') {
                        result = undefined;
                    }
                    this.put(x, y, result);
                }
            }
        }
    };
};

CanvasExperiment2 = function() {
    var p;
    var canvas;
    var ctx;
    var intervalId;
    var w = 40;
    var h = 40;

    var colors = ["red", "black", "green", "blue"];

    this.draw = function() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        var c = colors[Math.floor(Math.random() * colors.length)];
        var x = Math.floor(Math.random() * 10);
        var y = Math.floor(Math.random() * 10);

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

    this.start = function(c) {
        p = new Playfield();
        canvas = c;
        ctx = canvas.getContext('2d');
        this.draw();
        intervalId = setInterval(this.draw, 25);
    }
}
