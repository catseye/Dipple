#!/usr/bin/env python

# ambinate.py: parameterized recursive {de,a}scent {parse,generato}r
# by Chris Pressey of Cat's Eye Technologies, sometime in
# 2015 or possibly earlier, see datestamp on:
# https://gist.github.com/cpressey/dd3f63eda91b33e429fa

import random
import re
import sys

# Sentence ::= NounPhrase VerbPhrase ".".
# NounPhrase ::= Noun ["and" NounPhrase].
# Noun := "the" ("cat" | "dog" | "mat").
# VerbPhrase ::= "sat" "on" NounPhrase.

def sentence(g):
    noun_phrase(g)
    verb_phrase(g)
    g.term(".")
    g.done()

def noun_phrase(g):
    noun(g)
    r = g.test(["and", ""])
    if r == 'and':
        g.term('and')
        noun_phrase(g)

def noun(g):
    g.term("the")
    r = g.test(["cat", "dog", "mat"])
    if r == 'cat':
        g.term("cat")
    if r == 'dog':
        g.term("dog")
    if r == 'mat':
        g.term("mat")

def verb_phrase(g):
    g.term("sat")
    g.term("on")
    noun_phrase(g)


class RecursiveScent(object):
    """This is an interface"""
    def term(self, s):
        raise NotImplementedError

    def test(self, l):
        raise NotImplementedError

    def done(self):
        raise NotImplementedError


class Generator(RecursiveScent):
    def term(self, s):
        sys.stdout.write(s + ' ')

    def test(self, l):
        return random.choice(l)

    def done(self):
        sys.stdout.write('\n')


class Parser(RecursiveScent):
    def __init__(self, s):
        self.s = s
        self.token = None
        self._scan()
        self.errors = []

    def _scan(self):
        self.s = self.s.lstrip()
        match = re.match(r'^(\w+|\.)(.*?)$', self.s)
        if match:
            self.token = match.group(1)
            self.s = match.group(2)
        else:
            self.token = None
            self.s = ""

    def term(self, s):
        if self.token == s:
            self._scan()
        else:
            self.errors.append('expected "%s", found "%s"' % (s, self.token))

    def test(self, l):
        if self.token in l:
            return self.token
        elif "" in l:  # nullable
            return ""
        else:
            self.errors.append('"%s" not found in %s' % (self.token, l))

    def done(self):
        if self.errors:
            sys.stdout.write('errors: %s\n' % self.errors)
        else:
            sys.stdout.write('well-formed sentence.\n')

### MAIN ###

sentence(Generator())

s = 'the dog and the mat sat on the cat and the cat .'
sentence(Parser(s))

s = 'the dog and some cheese sat on the cat and the cat .'
sentence(Parser(s))

