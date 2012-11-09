function MouseTracker(elem) {
    var self = {};
    var events = [];

    self.start = function() {
        elem.onmousemove = function(event) {
            events.push(event);
        };
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

    self.dump = function() {
        var i;
        var base = events[0].timeStamp;
        var offs = self.offset();
        for (i = 0; i < events.length; i++) {
            var x = events[i].pageX - offs[0];
            var y = events[i].pageY - offs[1];
            var sec = (events[i].timeStamp - base) / 1000.0;
            console.log("" + x + ", " + y + " @ " + sec);
        }
        events = [];
    };

    return self;
}
