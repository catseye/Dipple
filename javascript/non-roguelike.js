// requires playfield.js
NonRoguelike = function() {
    var map;
    var actors;
    var canvas;
    var ctx;
    var intervalId;
    var heroX = 2;
    var heroY = 2;
    var gold = 0;
    var w = 40;
    var h = 40;
    var counter = 0;
    var state = undefined;

    var WALL = {
        'draw': function(ctx, x, y) {
            ctx.fillStyle = "black";
            ctx.fillRect(x * w, y * h, w, h);
        },
        'pass': function(x, y) {
            return false;
        }
    };

    var DOOR = {
        'draw': function(ctx, x, y) {
            ctx.fillStyle = "brown";
            ctx.fillRect(x * w, y * h, w, h);
        },
        'pass': function(x, y) {
            return true;
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
        'pass': function(x, y) {
            map.put(x, y, undefined);
            gold += 10;
            return true; 
        }
    };

    var HERO = {
        'draw': function(ctx, x, y) {
            ctx.beginPath();
            ctx.fillStyle = "blue";
            ctx.arc(x * w + (w / 2), y * h + (h / 2), w/2, 0, 2 * Math.PI, false);
            ctx.fill();
        }
    };

    var MONSTER = {
        'draw': function(ctx, x, y) {
            ctx.beginPath();
            ctx.fillStyle = "green";
            ctx.arc(x * w + (w / 2), y * h + (h / 2), w/2, 0, 2 * Math.PI, false);
            ctx.fill();
        }
    };

    this.draw = function() {
        /*
        w = 35 + Math.sin(counter) * 5;
        h = 35 + Math.sin(Math.PI + counter) * 5;
        counter += 0.05;
        */
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        map.foreach(function (x, y, thing) {
            thing.draw(ctx, x, y);
        });
        actors.foreach(function (x, y, actor) {
            actor.draw(ctx, x, y);
        });
    }

    this.moveHero = function(dx, dy) {
        var newHeroX = heroX + dx;
        var newHeroY = heroY + dy;
        var thing = map.get(newHeroX, newHeroY);
        var pass = true;
        if (thing !== undefined) {
            pass = thing.pass(newHeroX, newHeroY);
        }
        var creature = actors.get(newHeroX, newHeroY);
        if (creature !== undefined) {
            document.getElementById('status').innerHTML = "FighT!";
            pass = false;
        }
        if (pass) {
            actors.put(heroX, heroY, undefined);
            heroX = newHeroX;
            heroY = newHeroY;
            actors.put(heroX, heroY, HERO);
            document.getElementById('status').innerHTML = "Gold: " + gold;
        }
    };

    this.start = function(c) {
        map = new Playfield();
        var legend = {
            ' ': undefined,
            '*': WALL,
            '$': GOLD,
            '+': DOOR
        };
        map.load(0, 0, legend,
          "***************\n" +
          "*    *        *\n" +
          "*    *        *\n" +
          "*    *  $     *\n" +
          "*** **        *\n" +
          "*** ********+**\n" +
          "*** **        *\n" +
          "***  +        *\n" +
          "******        *\n" +
          "***************\n"
        );

        actors = new Playfield();
        actors.put(heroX, heroY, HERO);
        actors.put(10, 2, MONSTER);

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
