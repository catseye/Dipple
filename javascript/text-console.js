/*
 * A text-based console simulation in Javascript on an HTML5 canvas.
 *
 * Note that I am not suggesting that this is a *good* thing in most
 * circumstances.  I mean, you have a GUI!  You have graphics!  Why would
 * you want to limit your interaction to a text-based console?  And you
 * can't even select text in it, and if you want to handle input you'll
 * have to write a bunch of stuff to do line editing and everything.
 *
 * But still, sometimes, for art's sake, what you want to do is
 * simulate a text-based console.  So be it.  You can use this.
 *
 * Create a new TextConsole object t, then call t.init(), then call
 * t.write() to write text to the console.
 *
 * You can also change the textColor and backgroundColor attributes
 * between calls to t.write().  You can call t.reset() to clear the
 * simulated screen (to the selected backgroundColor.)  You can also set
 * or clear overStrike mode.
 *
 * TODO: display a blinking cursor (and allow it to be turned off.)
 */
TextConsole = function() {
  this.canvas = null;
  this.charHeight = null;
  this.charWidth = null;
  this.rows = null;
  this.cols = null;
  this.row = null;
  this.col = null;
  this.overStrike = null;
  this.textColor = null;
  this.backgroundColor = null;
  
  this.blinkInterval = null;

  /*
   * Attach a canvas to this TextConsole.  The canvas will
   * be resized to match the given dimensions.
   */
  this.init = function(canvas, charHeight, cols, rows) {
    this.canvas = canvas;
    this.charHeight = charHeight;
    this.rows = rows;
    this.cols = cols;

    var ctx = this.canvas.getContext('2d');
    ctx.font = this.charHeight + "px monospace";
    this.charWidth = ctx.measureText("@").width;

    this.textColor = "green";
    this.backgroundColor = "black";
    this.reset();
  };

  /*
   * Resize the TextConsole to match the given dimensions,
   * clear it to the current backgroundColor, turn off
   * overstrike mode, and home the cursor.
   */
  this.reset = function() {
    this.overStrike = false;
    this.row = 0;
    this.col = 0;
    this.canvas.width = this.charWidth * this.cols;
    this.canvas.height = this.charHeight * this.rows;
    var ctx = this.canvas.getContext('2d');
    ctx.fillStyle = this.backgroundColor;
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    if (this.blinkInterval !== null) {
      clearInterval(this.blinkInterval);
      this.blinkInterval = setInterval(function() {
      }, 500);
  };

  /*
   * Advance the cursor to the next line, scrolling the
   * TextConsole display if necessary.
   */
  this.advance = function() {
    this.col = 0;
    this.row += 1;
    var ctx = this.canvas.getContext('2d');
    while (this.row >= this.rows) {
      var imgData = ctx.getImageData(
          0, this.charHeight, canvas.width, canvas.height - this.charHeight
      );
      ctx.putImageData(imgData, 0, 0);
      ctx.fillStyle = this.backgroundColor;
      ctx.fillRect(
          0, canvas.height - this.charHeight, canvas.width, this.charHeight
      );
      this.row -= 1;
    }
  };

  /*
   * Write a string to the TextConsole.  Line feeds will cause a
   * new line, and backspaces will move the cursor left if it is not
   * already at the left edge.
   */
  this.write = function(string) {
    var i = 0;
    var ctx = this.canvas.getContext('2d');
    ctx.textBaseline = "top";
    ctx.font = this.charHeight + "px monospace";
    while (i < string.length) {
      var c = string.charAt(i);
      if (c === '\n') {
        this.advance();
      } else if (c === '\b' && this.col > 0) {
        this.col--;
      } else if (c >= ' ') {
        if (!this.overStrike) {
          ctx.fillStyle = this.backgroundColor;
          ctx.fillRect(this.col * this.charWidth, this.row * this.charHeight,
                       this.charWidth, this.charHeight);
        }
        ctx.fillStyle = this.textColor;
        ctx.fillText(c, this.col * this.charWidth, this.row * this.charHeight);
        this.col += 1;
        if (this.col >= this.cols) {
          this.advance();
        }
      }
      i++;
    };
  };

  /*
   * Move the cursor around the TextConsole.  x is the column number
   * (0-based) and y is the row number (also 0-based.)
   */
  this.gotoxy = function(x, y) {
    this.col = x;
    this.row = y;
  };

  /*
   * DEMONSTRATION ONLY of capturing keypresses and writing them to the
   * console.
   */
  this.hookUpKeyboardInput = function(object) {
    var t = this; 
    object.addEventListener('keyup', function(e) {
      switch (e.keyCode) {
        case 38:  /* Up arrow */
          break;
        case 40:  /* Down arrow */
          break;
        case 37:  /* Left arrow */
          break;
        case 39:  /* Right arrow */
          break;
      }
    }, true);
    object.addEventListener('keypress', function(e) {
      t.write(String.fromCharCode(e.charCode));
    }, true);
  }
};

