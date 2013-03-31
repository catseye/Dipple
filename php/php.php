<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>PHP PHP PHP</title>
  </head>
  <body>

<?php 

/* comment */
// comment
# comment

echo "Hello, World!<br>\n";
ini_set('display_errors', '1');

// data types

$var = 24000;     // integer
$var = 0xff7fff;  // also integer
$var = 2.73;      // fp
$var = "line\nback\\slash\ndollar\$sign\x5a";  // string
$var = "interpolate $var"; // does not appear to work with []'s?
$var = <<<EOS
This here is a here-doc.  Three arrows!  Three!
EOS;

$var = "a";
$var{2} = "d";
$var{1} = "n";  // $a == "and"
$var = $var . "i";
print $var;
print $var{2};  // d
print "\n";

$var = "Hello from var<br>\n";
// cannot be seen inside functions, so do this there:
print $GLOBALS['var'];

// terrible!  awful!
$indirect = 'var';
print $$indirect;

if (isset($indirect)) { print "Indirect is defined<br>\n"; }
unset($indirect);
if (!isset($indirect)) { print "Now it's not<br>\n"; }
$indirect = NULL;
if (!isset($indirect)) { print "Still not<br>\n"; }
if (empty($indirect)) { print "And it's empty<br>\n"; }
$indirect = false;
if (empty($indirect)) { print "Defined but still empty<br>\n"; }

// SUPERGLOBALS

// $_GET[]
// $_POST[]
// $_COOKIE[]
// $_ENV[]
// $_SERVER[]

if (0) {} else { print "0 is falsey\n"; }
if (100) { print "100 is truthy\n"; }
if ("") {} else { print "\"\" is falsey\n"; }
if ("0") {} else { print "\"0\" is falsey\n"; }
if ("00") { print "\"00\" is truthy\n"; }
// similar to js
if (0 === 0) { print "0 is identical to 0\n"; }
if (0 === "0") {} else { print "0 is not identical to \"0\"\n"; }

// arrays.  Think lua; they're always tables.

$a = array(1, 2, 3);
$a = array(1 => 1, 2 => 2, 3 => 3);
$a[] = "9";
print_r($a);
$a = array(100 => 'a', 'b', 'c', 'd');
if (array_key_exists(102, $a)) {
    print $a[102]; print "\n";
}
$a = array('h' => 'aitch', 'b' => 'bee', 'r' => 'arr');
print $a['h'] . "\n";

print_r($a);

// modify in loop. NOTE PHP FULLY SUPPORTS SCOPE FAIL:
// if you say &$value instead of a new name here,
// the lexically next loop will mess up, because it
// uses $value.
foreach ($a as $key => &$vabb) {
  if ($key == 'b') { $vabb = 'BEE'; }
}

foreach ($a as $key => $value) {
  print $key . '=' . $value . "\n";
}

reset($a);
while (list($key, $val) = each($a)) {
  print "$key = $val\n";
}

// list as LHS
list($hi, $there) = array("Hi", "there!");
print "$hi $there\n";

// include a define
include 'definitions.php';
print MY_CONSTANT;
// include $_SERVER["DOCUMENT_ROOT"] . "/myscript.php";

// references.  eeggh
$foo = "hi";
$bar =& $foo;  // $bar now aliases $foo
$bar = "lo";
print $foo;    // prints lo
print "\n";
// $bar =& "you can't do that on television, or in php";

// dear god, casts, with many names
print 100 + 100;
print "100" + "100";
print (int)"100" + (integer)"100";
print "\n";
print (bool)"100" + (boolean)"100";
print "\n";

// "The operator @ silences error messages during the evaluation process of an
// expression."  I hate you, PHP.

// round these parts it's called "elseif" (not "elif", not "elsif").
// but just follow C, it's fine.

$a = 7;
if ($a > 10) {
  print "no";
} else if ($a > 5) {
  print "yes\n";
} else {
  print "no";
}

// omg you can also use endif

if ($a < 2):
  print "yup\n";
else:
  print "nope\n";
endif;

// JUMP IN AND OUT OF PHP
// note, does not expand $num in html
 
 $num =& $a;
 if ($num < 0): ?>
<h1>$num is negative</h1>
<?php elseif($num == 0): ?>
<h1>$num is zero</h1>
<?php elseif($num > 0): ?>
<h1>$num is positive</h1>
<?php endif;

// switch
// NB: "The match is done internally using the regular equality
// operator (==), not the identical operator (===)"
switch ($a) {
  case 7:
    print "yes, I know\n";
  case 8:
    print "wtf fallthrough\n";
    break;
  default:
    print "OTHER\n";
}

// of course, ... use "break" and "continue", and endwhile if you must
$i = 5;
$d = 1;
while ($i > 0) {
  $d *= $i--;
}
print "5! = $d\n";

$i = 5;
$d = 1;
do {
  $d *= $i--;
} while ($i > 0);
print "5! = $d\n";

// yes, you can
for ($i = 0; $i < 10; $i++) {
  print $i % 2 ? "$i is odd\n" : "\$i is $i\n";
}
for ($i = 'fob'; $i <= 'fog'; $i++) {
  print "\$i is $i\n";
}

// eval CAN INJECT INTO SCOPE. USE IT, ALWAYS.
eval('$vbbz = 7;');
print "$vbbz\n";

// finally, we get to functions.

function last($vbbz) {
  return $vbbz{strlen($vbbz)-1};
}

print "vbbz is still $vbbz\n";
print "last garlic is " . last('garlic') . "\n";

// can't redefine functions. new name pls
function lastz($vbbz) {
  $GLOBALS['vbbz'] = 'whee';
  return $vbbz{strlen($vbbz)-1};
}

print "vbbz is $vbbz\n";
print "lastz spoon is " . lastz('spoon') . "\n";
print "vbbz is $vbbz\n";

// hoy it's like python. sorta
function lastq($vbbz) {
  global $vbbz;
  return $vbbz{strlen($vbbz)-1};
}

print "lastq orc is " . lastz('orc') . "\n";

// functions can OMG return references

function &g($x) {
    return $GLOBALS[$x];
}

$x = &g('var');
$x = 23;
print "$var\n";

// to locals? yes,  but there's no point
function &gg($x) {
  $c = $x;
  return $c;
}
$x = &gg('var');
$x = 12;
print "$x\n";

// can pass  by reference.  also default value
function add(&$dest, $value=1) {
  $dest += $value;
}
//add(23, 10);
add($x, 10);
print "$x\n";
add($x);
print "$x\n";

// static
function adds(&$dest, $value=NULL) {
  static $incr = 0;
  if (isset($value)) { $incr = $value; }
  $dest += $incr;
  print "adds: dest is now $dest\n";
}

$x = 0;
adds($x);
adds($x, 5);
adds($x);

// OBJECT ORIENTATTATATTATATTATATTTION

interface IGreeter {  // extends YetAnotherIFace, YetAnotherAnotherIFace {
  function hello();
}

// DOESN'T NEED TO ABSTRACT hello() EVEN THOUGH IT ... YEAH
abstract class BaseCounter implements IGreeter {
  abstract function zero();
  abstract function incr();
}

class Counter extends BaseCounter {
  //public $value;
  //private $value;
  protected $value;
  const INCR = 1;  // class-local constant.  can't be private!!!?!

  function __construct($value) {
    $this->value = $value;
  }

  function zero() {
    $this->value = 0;
  }

  function incr() {
    $this->value += self::INCR;
  }

  final function getValue() {  // can't be overridden
    return $this->value;
  }

  /* "The __toString() method is currently only called by the print and echo
  language constructs."  Chrissakes, PHP... uggh. */
  function __toString() {
    return "Counter(" . $this->getValue() . ")";  // can't leave off $this->
  }

  function __destruct() {
    print "bye\n";
  }

  function hello() {
    print "hello from a " . __CLASS__  . "!\n";
  }
}

$c = new Counter(700);
print "counter is now " . $c . "\n";
$c->zero();
//print "counter is " . $c->value . "\n";
// u can make up members
$c->name = "Allie";
print "counter's name is " . $c->name . "\n";
$c->incr();
$d = $c;   // shallow copy obj ref
$d->incr();
$c->incr();
print "counter is now " . $c . "\n";  // 3

final class ParaCounter extends Counter {  // can't be subclassed
  private static $howManyOfTheseExist = 0;
  private $incval;

  function __construct($value, $incval) {
    parent::__construct($value);
    $this->incval = $incval;
    ParaCounter::$howManyOfTheseExist++;
    $this->hello();
    print "I'm a " . __CLASS__ . "!\n";
  }

  static function howmany() {
    //return ParaCounter::$howManyOfTheseExist;
    return self::$howManyOfTheseExist;
  }

  function incr() {
    $this->value += $this->incval;
  }
}

$r = new ParaCounter(0, 5);
$c = new ParaCounter(60, 5);
print "counter is now " . $c->getValue() . "\n";
$c->zero();
$c->incr();
$c->incr();
$c->incr();
print "counter is now " . $c->getValue() . "\n";
print "howmany? " . $c->howmany();
print "howmany? " . $r::howmany();
// nope, private
// print ParaCounter::$howManyOfTheseExist;
print "\n";
print "Is r a IGreeter? " . ($r instanceof IGreeter) . "\n";

function doGreeter(IGreeter $x) {
  $x->hello();
}

doGreeter($r);
// doGreeter(23); // fail

class BadOne extends Exception {
  function fizzle() {
    print "fizzle\n";
  }
}

// ceptions
try {
  $f = new BadOne();
  throw $f;
} catch (IGreeter $e) {
  print "naw\n";
} catch (BadOne $e) {
  $e->fizzle();
}

// mmmmmmagic members
class Container {
    function __get($property) {
        return $property;
    }
    function __set($property, $value) {
        print "$property=$value\n";
    }
    function __call($method, $args) {
        print "$method!\n";
        print_r($args);
    }
}

$c = new Container();
print $c->fahahaha . "\n";
$c->wahahaha = 'blahahaha';
$c->blahahaha($c, 'moo', 23);

// ok, what happens if...
function bold($s) {
    ?> <b> <?= $s ?> </b> <!-- haha --> <?php
}
bold("yes");
bold("no");
bold("wot?");
// HAHA it "works"!

if (in_array("foo", array("bar", "foo", "quuz"))) {
  print "yup it's there\n";
}

// myqsl junk

/*
mysql_connect("localhost", "root", "root");
mysql_select_db("test");
$query = mysql_query("SELECT name, age FROM players");
echo "The table currently contains " . mysql_num_rows($query) . " row(s)<br>";
while($row = mysql_fetch_array($query)) {
  echo $row["name"] . " is aged " . $row["age"] . "<br>";
}
$b = array("name" => "John", "age" => 28);
echo $b['name'] . " is " . $b['age'] . " years old <br>"; 
foreach ($b as $key => $value) {
  print "<p>#$key = $value</p>";
}
*/

?> 
 
  <p><?= $vbbz ?></p>
 
 </body>
</html>
