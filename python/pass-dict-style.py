# New Python programming style!
# Never return values.
# Always pass around a dict instead.

def foo(v):
    v['sum'] = v['a'] + v['b']
    v['rat'] = v['a'] / v['b']

def main():
    v = dict(a=10, b=1)
    foo(v)
    print(v['sum'])
    v['b'] = 0
    try:
        foo(v)
    except:
        pass
    print(v['sum'])

main()
