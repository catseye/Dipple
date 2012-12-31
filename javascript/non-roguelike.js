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

    this.load = function(x, y, map, string) {
        var lx = x;
        var ly = y;
        for (var i = 0; i < string.length; i++) {
            var c = string.charAt(i);
            if (c === '\n') {
                lx = x;
                ly++;
            } else if (c === ' ') {
                this.put(lx, ly, undefined);
                lx++;
            } else if (c === '\r') {
            } else {
                this.put(lx, ly, map[c]);
                lx++;
            }
        }
    };
};

NonRoguelike = function() {
    var p;
    var canvas;
    var ctx;
    var intervalId;
    var heroX = 2;
    var heroY = 2;
    var gold = 0;
    var w = 40;
    var h = 40;

    var WALL = {
        'draw': function(ctx, x, y) {
            ctx.beginPath();
            ctx.fillStyle = "black";
            ctx.fillRect(x * w, y * h, w, h);
        },
        'pass': function() { return false; }
    };

    var HERO = {
        'draw': function(ctx, x, y) {
            ctx.beginPath();
            ctx.fillStyle = "blue";
            ctx.arc(x * w + (w / 2), y * h + (h / 2), w/2, 0, 2 * Math.PI, false);
            ctx.fill();
        }
    };

    var GOLD = {
        'draw': function(ctx, x, y) {
            ctx.beginPath();
            ctx.fillStyle = "yellow";
            ctx.lineTo(x * w, y * h + h);
            ctx.lineTo(x * w + (w/2), y * h + (h/2));
            ctx.lineTo(x * w + w, y * h + h);
            ctx.closePath();
            ctx.fill();
        },
        'pass': function() { gold += 10; return true; }
    };

    this.draw = function() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        p.foreach(function (x, y, thing) {
            thing['draw'](ctx, x, y);
        });
    }

    this.moveHero = function(dx, dy) {
        var newHeroX = heroX + dx;
        var newHeroY = heroY + dy;
        var thing = p.get(newHeroX, newHeroY);
        var pass = (thing === undefined) ? true : thing['pass']();
        if (pass) {
            p.put(heroX, heroY, undefined);
            heroX = newHeroX;
            heroY = newHeroY;
            p.put(heroX, heroY, HERO);
            document.getElementById('status').innerHTML = "Gold: " + gold;
        }
    };

    this.start = function(c) {
        p = new Playfield();
        var map = {
            ' ': undefined,
            '*': WALL,
            '$': GOLD
        };
        p.load(0, 0, map,
          "***************\n" +
          "*    *        *\n" +
          "*    *        *\n" +
          "*    *  $     *\n" +
          "*** **        *\n" +
          "*** ******** **\n" +
          "*** **        *\n" +
          "***           *\n" +
          "******        *\n" +
          "***************\n"
        );

        p.put(heroX, heroY, HERO);

        canvas = c;
        ctx = canvas.getContext('2d');
        var self = this;

        canvas.addEventListener('keydown', function(e) {
          switch (e.keyCode) {
            case 38:  /* Up arrow */
              self.moveHero(0, -1);
              e.cancelBubble = true;
              break;
            case 40:  /* Down arrow */
              self.moveHero(0, 1);
              e.cancelBubble = true;
              break;
            case 37:  /* Left arrow */
              self.moveHero(-1, 0);
              e.cancelBubble = true;
              break;
            case 39:  /* Right arrow */
              self.moveHero(1, 0);
              e.cancelBubble = true;
              break;
          }
        }, true);

        intervalId = setInterval(this.draw, 25);
    }
}
