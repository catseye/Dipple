#!/usr/bin/env nodejs

var args = process.argv.slice(2);

process.stdin.resume();
process.stdin.setEncoding('utf8');

var processLine = function(line) {
    process.stdout.write(args.join('') + line + "\n");
};

var buffer = "";

process.stdin.on('data', function(data) {
    data = data.replace(/\r/g, '');
    var lines = data.split("\n");
    console.log(lines);

    lines[0] = buffer + lines[0];
    buffer = lines[lines.length - 1];

    for (var i = 0; i < lines.length - 1; i++) {
        processLine(lines[i]);
    }
});

process.stdin.on('end', function() {
    processLine(buffer);
});
