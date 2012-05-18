#!/usr/bin/env python

import curses

if __name__ == '__main__':
    w = curses.initscr()
    curses.start_color()
    curses.init_pair(1, curses.COLOR_RED, curses.COLOR_BLACK)
    w.attron(curses.color_pair(1))
    w.move(12, 20)
    w.addstr("Hello there")
    w.attroff(curses.color_pair(1))
    w.getch()
    curses.endwin()
