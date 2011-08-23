<html>
<h1><?php

    $id = $_GET['id'];
    if ($id < 20) {
        echo "$id is less";
    } else {
        echo "$id is more";
    }
?></h1>

<p style="colour: red;">This is some text.
<?php

define("myCatName", "James the Cat");

class Cat
{
    public $name = myCatName;
}

$james = new Cat;
$puff = $james;
$puff->name = "Puff the Cat";

echo "James' name is $james->name.  ";
echo "Puff's name is $puff->name.";

?></p>
</html>
