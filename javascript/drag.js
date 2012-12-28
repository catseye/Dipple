Drag = function() {
    var interval_id = undefined;
    var ctx = undefined;
    var canvas = undefined;
    var selected = undefined;

    var boxes = [
      [100, 100, 60, 60, "green"],
      [200, 100, 100, 40, "red"],
      [100, 200, 50, 90, "yellow"]
    ];

    var hit = function(pt_x, pt_y, box) {
        return (pt_x >= box[0] &&
                pt_x < box[0] + box[2] &&
                pt_y >= box[1] &&
                pt_y < box[1] + box[3]);
    };

    this.init = function(c) {
      canvas = c;
      ctx = canvas.getContext("2d");

      canvas.onmousedown = function(e) {
        var can_x = e.pageX - canvas.offsetLeft;
        var can_y = e.pageY - canvas.offsetTop;
        for (var i = boxes.length-1; i >= 0; i--) {
          var box = boxes[i];
          if (hit(can_x, can_y, box)) {
            selected = i;
            var off_x = box[0] - can_x;
            var off_y = box[1] - can_y;
            canvas.onmousemove = function(e) {
                box[0] = (e.pageX - canvas.offsetLeft) + off_x;
                box[1] = (e.pageY - canvas.offsetTop) + off_y;
            }
            break;
          }
        }
      }
      canvas.onmouseup = function() {
        canvas.onmousemove = null;
        selected = undefined;
      };
      interval_id = setInterval(this.draw, 20);
    };

    this.draw = function() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      ctx.shadowOffsetX = 10;
      ctx.shadowOffsetY = 10;
      ctx.shadowBlur = 15;

      for (var i = 0; i < boxes.length; i++) {
        var box = boxes[i];
        ctx.shadowColor = "black";
        if (i === selected) {
          var max = box[2] > box[3] ? box[2] : box[3];
          var gradient = ctx.createLinearGradient(
            box[0], box[1], box[0]+max, box[1]+max
          );
          gradient.addColorStop(0, box[4]);
          gradient.addColorStop(0.5, "white");
          gradient.addColorStop(1, box[4]);
          ctx.fillStyle = gradient;
        } else {
          ctx.fillStyle = box[4];
        }
        ctx.fillRect(box[0], box[1], box[2], box[3]);

        ctx.lineWidth = "1";
        ctx.strokeStyle = "black";
        ctx.shadowColor = "transparent";
        ctx.strokeRect(box[0], box[1], box[2], box[3]);
      }
    };

};
