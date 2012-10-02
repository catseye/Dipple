var something = 5;

importScripts('worker-library.js');

postMessage("Hello!\n");

addEventListener('message', function(e) {
    if (e.data[0] === 'start') {
        postMessage("started\n");
        something += e.data[1];
        setInterval(function() {
            something = closure(something);
            postMessage("Value: " + something + "\n");
        }, 100);
    }
});
