function EsolangController(canvas) {
    var self = {};
    var interval_id;

    var p = Playfield();
    var ip_x;
    var ip_y;
    var ip_dx;
    var ip_dy;

    self.draw = function() {
        var ctx = canvas.getContext('2d');

        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.textBaseline = "top";

        var height = 20;
        ctx.font = height + "px monospace";
        var width = ctx.measureText("@").width;

        ctx.fillStyle = "#50ff50";
        ctx.fillRect(ip_x * width, ip_y * height, width, height);

        ctx.fillStyle = "black";
        p.foreach(function (x, y, value) {
            ctx.fillText(value, x * width, y * height);
        });
    }

    self.step = function() {
        var instr = p.get(ip_x, ip_y);

        if (instr === '@') {
            ip_dx = 1;
            ip_dy = 0;
        } else if (instr === '#') {
            ip_dx = 0;
            ip_dy = 1;
        }

        ip_x += ip_dx;
        ip_y += ip_dy;
        self.draw();
    }

    self.start = function() {
        if (interval_id !== undefined)
            return;
        self.step();
        interval_id = setInterval(self.step, 333);
    }

    self.stop = function() {
        if (interval_id === undefined)
            return;
        clearInterval(interval_id);
        interval_id = undefined;
    }

    self.load = function(textarea) {
        self.stop();
        p.load(0, 0, textarea.val());
        p.foreach(function (x, y, value) {
            if (value === '$') {
                ip_x = x;
                ip_y = y;
            }
        });
        ip_dx = 1;
        ip_dy = 0;
        self.draw();
    }

    return self;
}
