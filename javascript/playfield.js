function Playfield() {
    var self = {}
    var store = {}
    self.min_x = undefined;
    self.min_y = undefined;
    self.max_x = undefined;
    self.max_y = undefined;

    /*
     * Cells are undefined if they were never written to.
     */
    self.get = function(x, y) {
        return store[x+','+y];
    }

    self.put = function(x, y, value) {
        if (self.min_x === undefined || x < self.min_x) self.min_x = x;
        if (self.max_x === undefined || x > self.max_x) self.max_x = x;
        if (self.min_y === undefined || y < self.min_y) self.min_y = y;
        if (self.max_y === undefined || y > self.max_y) self.max_y = y;
        store[x+','+y] = value;
    }

    /*
     * Load a string into the playfield.
     * The string may be multiline, with newline (ASCII 10)
     * characters delimiting lines.  ASCII 13 is ignored.
     * A space in the string does not write anything into
     * the playfield.
     */
    self.load = function(x, y, string) {
        var lx = x;
        var ly = y;
        for (var i = 0; i < string.length; i++) {
            var c = string.charAt(i);
            if (c === '\n') {
                lx = x;
                ly++;
            } else if (c === '\r') {
            } else if (c === ' ') {
                lx++;
            } else {
                self.put(lx, ly, c);
                lx++;
            }
        }
    }

    /*
     * fun is a callback which takes three parameters:
     * x, y, and value.
     * This function ensures a particular order.
     */
    self.foreach = function(fun) {
        for (var y = self.min_y; y <= self.max_y; y++) {
            for (var x = self.min_x; x <= self.max_x; x++) {
                var key = x+','+y;
                var value = store[key];
                if (value === undefined)
                    continue;
                fun(x, y, value);
            }
        }
    }

    return self;
}
