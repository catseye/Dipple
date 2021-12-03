function allPermutations(items) {
    var item;
    var subItems;
    var permutations = [];
    for (var i = 0; i < items.length; i++) {
        item = items[i];
        subItems = items.slice();
        subItems.splice(i, 1);
        if (subItems.length > 0) {
            var subPermutations = allPermutations(subItems);
            for (var j = 0; j < subPermutations.length; j++) {
                var newItems = [item].concat(subPermutations[j]);
                permutations.push(newItems);
            }
        } else {
            permutations.push([item]);
        }
    }
    return permutations;
}