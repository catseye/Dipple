SideScroller = function() {
    var interval_id = undefined;
    var ctx = undefined;
    var canvas = undefined;
    var tileWidth = 100;
    var tileHeight = 100;
    var scrollOffset = 0;
    var shipX = 50;
    var shipY = 200;
    var delta = 0;

    var tileMap = [
      [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0],
      [1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 1, 1, 1, 1, 1, 1, 0, 1, 1, 2, 3, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0],
      [1, 1, 1, 1, 1, 1, 1, 2, 0, 0, 0, 3, 2, 0, 1, 1, 1, 2, 0, 0, 0, 3, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0],
      [1, 1, 1, 1, 1, 1, 2, 0, 0, 1, 0, 0, 0, 0, 3, 2, 0, 0, 1, 1, 1, 0, 3, 0, 0, 0, 0, 0, 3, 1, 2, 0, 0, 0, 0, 0, 0]
    ];

    var drawTile = function(tileId, x, y) {
        switch (tileId) {
          case 0:
            ctx.fillStyle="brown";
            ctx.fillRect(x, y, tileWidth, tileHeight);
            break;
          case 1:
            ctx.fillStyle="green";
            ctx.fillRect(x, y, tileWidth, tileHeight);
            break;
          case 2:
            ctx.fillStyle="brown";
            ctx.fillRect(x, y, tileWidth, tileHeight);
            ctx.beginPath();
            ctx.fillStyle="green";
            ctx.moveTo(x, y);
            ctx.lineTo(x + tileWidth, y);
            ctx.lineTo(x, y + tileHeight);
            ctx.closePath();
            ctx.fill();
            break;
          case 3:
            ctx.fillStyle="brown";
            ctx.fillRect(x, y, tileWidth, tileHeight);
            ctx.beginPath();
            ctx.fillStyle="green";
            ctx.moveTo(x, y);
            ctx.lineTo(x + tileWidth, y);
            ctx.lineTo(x + tileWidth, y + tileHeight);
            ctx.closePath();
            ctx.fill();
            break;
        }
    }

    var checkCollide = function(tileId, inTileX, inTileY) {
        switch (tileId) {
          case 0:
            return true;
          case 1:
            return false;
          case 2:
            return inTileX >= (tileHeight - inTileY);
          case 3:
            return inTileX <= inTileY;
        }
    }
    
    this.init = function(c) {
      canvas = c;
      ctx = canvas.getContext("2d");
      interval_id = setInterval(this.draw, 20);

      c.addEventListener('keydown', function(e) {
        switch (e.keyCode) {
          case 38:  /* Up arrow */
            delta = -1;
            e.cancelBubble = true;
            break;
          case 40:  /* Down arrow */
            delta = 1;
            e.cancelBubble = true;
            break;
        }
      }, true);

      c.addEventListener('keyup', function(e) {
        switch (e.keyCode) {
          case 38: case 40:
            delta = 0;
            e.cancelBubble = true;
            break;
        }
      }, true);

    };

    this.draw = function() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      var status = document.getElementById('status');
      var leftmost = Math.floor((0 - scrollOffset) / tileWidth);
      var rightmost = leftmost + 7; // 600 / tileWidth + 1 for overlap
      //status.innerHTML = "leftmost= " + leftmost + ", rightmost=" + rightmost;
      var shipTileX =  Math.floor((shipX - scrollOffset) / tileWidth);
      var shipTileY =  Math.floor(shipY / tileHeight);
      var shipTile = tileMap[shipTileY][shipTileX];
      var inTileX = (shipX - scrollOffset) - (shipTileX * tileWidth);
      var inTileY = shipY - (shipTileY * tileHeight);
      status.innerHTML = "ship tile X=" + shipTileX + ", Y=" + shipTileY + ", tile=" + shipTile + ", inTileX=" + inTileX + ", inTileY=" + inTileY;

      for (var y = 0; y < tileMap.length; y++) {
        var row = tileMap[y];
        for (var x = leftmost; x < rightmost; x++) {
          drawTile(row[x], x * tileWidth + scrollOffset, y * tileHeight);
        }
      }

      ctx.beginPath();
      ctx.fillStyle = "yellow";
      if (checkCollide(shipTile, inTileX, inTileY)) {
        ctx.fillStyle = "red";
      }
      ctx.arc(shipX, shipY, 12, 0, 2 * Math.PI, false);
      ctx.fill();

      scrollOffset -= 1;
      shipY += delta * 2;
    };

};
