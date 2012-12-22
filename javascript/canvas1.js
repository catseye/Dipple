function CanvasExperiment1(canvas) {
    var self = {};
    var counter;
    var interval_id;

    self.draw = function() {
      var ctx = canvas.getContext('2d');

      document.getElementById('counter').innerHTML = counter;

      ctx.clearRect(0, 0, canvas.width, canvas.height);

      ctx.beginPath();
      ctx.lineWidth = "1";
      ctx.strokeStyle = "black";
      ctx.moveTo(0,0);
      var y = 200;
      var w = (canvas.width / counter);
      for (var i = 1; i <= counter; i++) {
          ctx.lineTo(w * i, y);
          y = (y == 200 ? 0 : 200);
      }
      ctx.lineTo(canvas.width,0);
      ctx.stroke();
      
      counter += 1;
    };

    self.start = function() {
      counter = 1;
      self.draw();
      interval_id = setInterval(self.draw, 50);
    };

    return self;
}
