function inherit(Child, Parent) {
    Child.prototype = new Parent();
    Child.prototype['super'] = Parent;
}

function Thing() {
    this.color = "yellow";
    this.init = function(weight) {
        this.weight = weight;
    }
    this.weigh = function() {
        alert("this " + this.color + " thing weighs " + this.weight);
    }
}
function Subthing() {
    this.color = "brown";
}
Subthing.prototype = new Thing();
