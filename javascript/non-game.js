NonGame = function() {
    var ctx = undefined;
    var canvas = undefined;
    var status = undefined;
    var gameState = undefined;
    var sqWidth = 60;
    var sqHeight = 40;
    var self = this;

    var doComputerTurn = function() {
      var x = Math.floor(Math.random() * 10);
      var y = Math.floor(Math.random() * 10);
      self.startDrawPiece("#e00030", x, y, "player_turn");
    };

    var setState = function(state) {
      gameState = state;
      if (state === "player_turn") {
        status.innerHTML = "Your turn!";
      } else if (state === "moving") {
        status.innerHTML = "...";
      } else if (state === "computer_turn") {
        status.innerHTML = "My turn!";
        doComputerTurn();
      }
    }
      
    this.init = function(c, s) {
      canvas = c;
      ctx = canvas.getContext("2d");
      status = s;
      var self = this;

      canvas.onmouseup = function(e) {
        if (gameState !== "player_turn") return;
        var can_x = e.pageX - canvas.offsetLeft;
        var can_y = e.pageY - canvas.offsetTop;
        var x = Math.floor(can_x / sqWidth);
        var y = Math.floor(can_y / sqHeight);
        setState("moving");
        self.startDrawPiece("#00e030", x, y, "computer_turn");
      };

      this.draw();
      setState("player_turn");
    };

    this.startDrawPiece = function(style, x, y, newState) {
      var interval_id;
      var self = this;
      var size = 0.0;
      interval_id = setInterval(function() {
        var last = false;
        if (size > 1.0) {
          size = 1.0;
          last = true;
        }
        ctx.beginPath();
        ctx.fillStyle = style;
        ctx.arc(x * sqWidth + (sqWidth / 2), y * sqHeight + (sqHeight / 2),
                size * (sqHeight / 2), 0, 2 * Math.PI, false);
        ctx.fill();
        if (last) {
          clearInterval(interval_id);
          setState(newState);
        } else {
          size += 0.1;
        }
      }, 20);
    };

    this.draw = function() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      for (var x = 0; x < 10; x++) {
        for (var y = 0; y < 10; y++) {
          ctx.fillStyle = (x + y) % 2 ? "black" : "white";
          ctx.fillRect(x * sqWidth, y * sqHeight, (x+1) * sqWidth, (y+1) * sqHeight);
        }
      }
    };

};
