#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Usage:
#  ./main.py test.c

import sys
import clang.cindex

debug = sys.stderr.write

def visit_node(node, indent=0):
  print('%s%s : %s' % ('  ' * indent, node.kind.name, node.spelling))
  for c in node.get_children():
    visit_node(c, indent=indent+1)

def show(n):
  print(n.kind)
  print(n.spelling)
  
def precond(func):
  spec = [([], [], [], [])]
  ch = func.get_children()
  decl = next(ch)
  body = next(ch)
  for s in body.get_children():
    show(s)

def analyze(filename):
  idx = clang.cindex.Index.create()
  tu = idx.parse(filename)
  topdecls = tu.cursor.get_children()
  for func in topdecls:
    debug(f"analysing {func.spelling}\n")
    precond(func)

def main():
  if len(sys.argv) < 2:
    print("pass the file")
  else:
    analyze(sys.argv[1])

if __name__ == '__main__':
  main()
else:
  analyze("test.c")
