function CanvasExperiment3(canvas, textarea, button) {
    var self = {};
    var p;
    var interval_id;

    self.draw = function() {
        var ctx = canvas.getContext('2d');

        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.textBaseline = "top";

        var height = 20;
        ctx.font = height + "px monospace";
        var width = ctx.measureText("@").width;

        p.foreach(function (x, y, value) {
            ctx.fillText(value, x * width, y * height);
            if (Math.random() > 0.95) {
                ctx.strokeStyle = "green";
                ctx.strokeRect(x * width, y * height, width, height);
            }
        });
    }

    self.start = function() {
        p = Playfield();
        self.draw();
        interval_id = setInterval(self.draw, 500);
        button.click(function() {
            p.load(0, 0, textarea.val());
            self.draw();
        });
    }

    return self;
}
