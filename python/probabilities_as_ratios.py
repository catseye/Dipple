# Maintaining probabilities as ratios
# and doing operations on them as ratios
# and showing that some algebraic laws do not hold

# ...............................
# ratios are expressed as: (n, d)

def mul_ratios(p1, p2):
    (n1, d1) = p1
    (n2, d2) = p2
    return (n1 * n2, d1 * d2)

def gcd_ratios(p1, p2):
    (n1, d1) = p1
    (n2, d2) = p2
    dc = d1 * d2
    n1c = n1 * d2
    n2c = n2 * d1
    return n1c, n2c, dc

def add_ratios(p1, p2):
    n1c, n2c, dc = gcd_ratios(p1, p2)
    return (n1c + n2c, dc)

def neg_ratio(p1):
    (n1, d1) = p1
    return (-n1, d1)

def equiv_ratios(p1, p2):
    n1c, n2c, dc = gcd_ratios(p1, p2)
    return n1c == n2c

# .....................................
# probabilities are expressed as ratios

def intersection(p1, p2):
    return mul_ratios(p1, p2)

def union(p1, p2):
    return add_ratios(add_ratios(p1, p2), neg_ratio(mul_ratios(p1, p2)))

# .....................................
# testing laws in a horrible manual way

def did_commute(op, p1, p2):
    r1 = op(p1, p2)
    r2 = op(p2, p1)
    if equiv_ratios(r1, r2):
        print("yes:", r1)
    else:
        print("no:", r1, r2)

def did_assoc(op, p1, p2, p3):
    r1 = op(op(p1, p2), p3)
    r2 = op(p1, op(p2, p3))
    if equiv_ratios(r1, r2):
        print("yes:", r1)
    else:
        print("no:", r1, r2)

def did_absorb(op1, op2, p1, p2):
    r1 = p1
    r2 = op1(p1, op2(p1, p2))
    if equiv_ratios(r1, r2):
        print("yes:", r1)
    else:
        print("no:", r1, r2)

def test():
    did_commute(union, (1, 6), (1, 10))
    did_commute(intersection, (1, 6), (1, 10))

    did_assoc(union, (1, 6), (1, 10), (1, 2))
    did_assoc(intersection, (1, 6), (1, 10), (1, 2))

    did_absorb(union, intersection, (1, 6), (1, 10))
