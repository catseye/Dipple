// requires playfield.js
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
    var counter = 0;

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
        w = 35 + Math.sin(counter) * 5;
        h = 35 + Math.sin(Math.PI + counter) * 5;
        counter += 0.05;
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
