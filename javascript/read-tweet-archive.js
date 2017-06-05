#!/usr/bin/env nodejs

// node.js script to read data from the archive that Twitter gives you when you select
// "Download an Archive of my Tweets".

// Run this from the base directory of the archive that was unzipped.

// As written, downloads the media for each tweet.  This requires wget.

var fs = require("fs");
var util = require("util");
var child_process = require("child_process");
var path = require("path");

var dataPath = "data/js/tweets";

Grailbird = {
    data: {}
};

files = fs.readdirSync(dataPath);

files.forEach(function(f) {
    require(process.cwd() + '/' + dataPath + '/' + f);
});

var downloads = [];

Object.keys(Grailbird.data).forEach(function(key) {
    Grailbird.data[key].forEach(function(tweet) {
        if (tweet.entities && tweet.entities.media) {
            tweet.entities.media.forEach(function(media) {
                downloads.push([tweet.id_str, media.media_url]);
            });
        }
    });
});

var delay = 0;

downloads.forEach(function(pair) {
    var url = pair[1];
    var dest = pair[0] + '_' + path.basename(url);

    if (fs.existsSync(dest)) {
        console.log(dest + ' already exists, skipping');
        return;
    }

    setTimeout(function() {
        var command = 'wget ' + url + ' -O ' + dest;
        console.log(command);
        child = child_process.exec(command, function (error, stdout, stderr) {
            console.log('stdout: ' + stdout);
            console.log('stderr: ' + stderr);
            if (error !== null) {
                console.log('exec error: ' + error);
            }
        });
    }, delay);

    delay += 1000;
});
