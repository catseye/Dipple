function MouseTracker(elem) {
    var self = {};
    var events = [];
    var base;

    self.start = function() {
        var offs = self.offset();
        elem.onmousemove = function(event) {
            if (events.length == 0) {
                events.push([event.pageX - offs[0], event.pageY - offs[1], 0.0]);
                base = event.timeStamp;
            } else {
                events.push([event.pageX - offs[0], event.pageY - offs[1],
                             (event.timeStamp - base) / 1000.0]);
            }
        };
        elem.style.background = "blue";
    };

    self.stop = function() {
        elem.onmousemove = null;
        elem.style.background = "white";
    };

    self.clear = function() {
        events = [];
    };

    self.offset = function() {
        var o = elem;
        var left = 0;
        var top = 0;

        while (o) {
            left += o.offsetLeft;
            top += o.offsetTop;
            o = o.offsetParent;
        }

        return [left, top];
    };

    self.replay = function() {
        var i = 0;
        var t = 0.0;
        var base = events[0].timeStamp;
        var offs = self.offset();
        var interval;
        interval = setInterval(function() {
            t += 0.01;
            s = "z";
            var x = undefined;
            var y = undefined;
            var sec;
            while (i < events.length && events[i][2] < t) {
                x = events[i][0];
                y = events[i][1];
                sec = events[i][2];
                s += "  (" + i + ") " + x + ", " + y + " @ " + sec;
                i++;
            }
            if (x !== undefined) {
                var ctx = elem.getContext("2d");
                ctx.clearRect(0, 0, elem.width, elem.height);
                ctx.fillStyle = "red";
                ctx.fillRect(x - 4, y - 4, 8, 8);
            }
            //document.getElementById('info').innerHTML = s;
            if (i >= events.length) {
                clearInterval(interval);
                return;
            }
        }, 10);
    };

    self.dump = function() {
        var e = document.getElementById('dump');
        var base = events[0].timeStamp;
        var offs = self.offset();

        var t = '[\n';
        for (var i = 0; i < events.length; i++) {
            t += '[' + events[i].toString() + ']';
            if (i < events.length - 1) { t += ','; }
            t += '\n';
        }
        t += ']';
        e.value = t;
    };

    self.load = function() {
        events = eval(document.getElementById('dump').value);
    };

    return self;
}
