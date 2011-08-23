<html>
<h1>ENV</h1>
<ul>
<?php
    foreach (@$_ENV as $k => $v) {
       echo "<li>$k = $v</li>";
    }
?>
</ul>
<h1>GET</h1>
<ul>
<?php
    foreach (@$_GET as $k => $v) {
       echo "<li>$k = $v</li>";
    }
?>
</ul>
<h1>POST</h1>
<ul>
<?php
    foreach (@$_POST as $k => $v) {
       echo "<li>$k = $v</li>";
    }
?>
</ul>
</html>
