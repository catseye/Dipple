function MouseTracker(elem) {
    var self = {};
    var events = [];

    self.start = function() {
        elem.onmousemove = function(event) {
            events.push(event);
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
            while (i < events.length && ((events[i].timeStamp - base) / 1000.0) < t) {
                x = events[i].pageX - offs[0];
                y = events[i].pageY - offs[1];
                sec = (events[i].timeStamp - base) / 1000.0;
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
            var x = events[i].pageX - offs[0];
            var y = events[i].pageY - offs[1];
            var sec = (events[i].timeStamp - base) / 1000.0;
            //for (var i = 0; i < path.length; i++) {
            t += [x, y, sec].toString();
            if (i < events.length - 1) { t += ','; }
            t += '\n';
        }
        t += ']';
        e.value = t;
    };

    self.load = function() {
    };

    return self;
}
