SideScroller = function() {
    var interval_id = undefined;
    var ctx = undefined;
    var canvas = undefined;
    var tileWidth = 100;
    var tileHeight = 100;
    var scrollOffset = 0;

    var tileMap = [
      [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1],
      [1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 1, 1, 1, 1, 1, 0, 1, 1],
      [1, 1, 1, 1, 1, 1, 1, 2, 0, 0, 0, 3, 2, 0, 1, 1, 2, 0],
      [1, 1, 1, 1, 1, 1, 2, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1]
    ];
    var tileMapWidth = 18;
    var tileMapHeight = 4;

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
      
    this.init = function(c) {
      canvas = c;
      ctx = canvas.getContext("2d");
      interval_id = setInterval(this.draw, 20);
    };

    this.draw = function() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      // TODO: optimize this to only draw the tiles visible on the Canvas
      for (var y = 0; y < tileMapHeight; y++) {
        var row = tileMap[y];
        for (var x = 0; x < tileMapWidth; x++) {
          drawTile(row[x], x * tileWidth + scrollOffset, y * tileHeight);
        }
      }

      scrollOffset -= 1;
    };

};
