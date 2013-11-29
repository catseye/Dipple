#!/usr/bin/env python

if __name__ == '__main__':
    octave = {}
    octave[7] = [34334, 36376, 38539, 40830, 43258, 45830, 48556, 51443, 54502, 57743, 61176, 64814]

    o = 6
    while o >= 0:
        octave[o] = [n / 2 for n in octave[o+1]]
        o -= 1

    print "freq_table_low:"
    o = 0
    while o <= 7:
        for n in octave[o]:
            print "    .byte", n % 256
        o += 1

    print ""
    print "freq_table_high:"
    o = 0
    while o <= 7:
        for n in octave[o]:
            print "    .byte", n / 256
        o += 1
