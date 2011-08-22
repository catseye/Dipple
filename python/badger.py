import sys
import getopt

def doTheThing(thing):
    sys.stdout.write(thing + "> ")
    line = sys.stdin.readline()
    while line != "quit\n":
        print "you typed " + line
        line = sys.stdin.readline()
        sys.stdout.write(thing + "> ")

def usage():
    print "usage:"
    print "  [python] " + sys.argv[0] + " [-g foo] [-h] ..."

def main(argv):
    try:
        opts, args = getopt.getopt(argv, "dg:h", ["grammar=", "help"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-g", "--grammar"):
            grammar = arg
        elif opt in ("-h", "--help"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1

    #source = "".join(args)
    for thing in args:
        k = doTheThing(thing)
    print k.output()
