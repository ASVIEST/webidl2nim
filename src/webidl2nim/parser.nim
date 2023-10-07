import ast
import ast_gen
import tokens

import npeg {.all.}
import npeg/codegen {.all.}

import grammar

import sequtils, sugar
import std/deques

type
  ParserState* = object
    stack*: Deque[Node]
    args: seq[Node]

proc push(p: var ParserState, n: Node) {.used.} =
  p.stack.addLast n

proc pop(p: var ParserState): Node {.used.} =
  popLast p.stack

proc popFirst(p: var ParserState): Node {.used.} =
  popFirst p.stack

template `%`(i: int): Node {.used.} =
  p.stack[^i]

template capture(n: Node) {.used.} =
  p.push(n)



template genIdents(cap: untyped)=
  if cap.len <= 2:
    capture ident($cap[1].s)
  else:
    var node = idents()
    for i in 1 ..< cap.len:
      node.add ident($cap[i].s)
    capture(node)

template popSameKindNodes(nodeKind: NodeKind): seq[Node] =
  var members: Deque[Node]
  while (%1).kind == nodeKind:
    members.addFirst p.pop()
    if p.stack.len <= 0:
      break
  members.toSeq

var p* = peg(Definitions, Token, p: ParserState):
  Definitions <- definitions.Definitions


proc parseCode*(t: openArray[Token]): ParserState=
  discard p.match(t, result)
