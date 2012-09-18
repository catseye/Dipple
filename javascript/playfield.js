function Playfield() {
    var self = {}
    var store = {}
    var min_x;
    var min_y;
    var max_x;
    var max_y;

    /*
     * Cells are undefined if they were never written to.
     */
    self.get = function(x, y) {
        return store[x+','+y];
    }

    self.put = function(x, y, value) {
        if (min_x === undefined || x < min_x) min_x = x;
        if (max_x === undefined || x > max_x) max_x = x;
        if (min_y === undefined || y < min_y) min_y = y;
        if (max_y === undefined || y > max_y) max_y = y;
        store[x+','+y] = value;
    }

    /*
     * fun is a callback which takes three parameters:
     * x, y, and value.
     */
    self.foreach = function(fun) {
        for (var y = min_y; y <= max_y; y++) {
            for (var x = min_x; x <= max_x; x++) {
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
