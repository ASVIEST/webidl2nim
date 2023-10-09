import options
import std/[sets, tables]
import ast
import tokens

type
  DeclDeps*[S] = object
    inheritance*: Option[S]
    usedTypes: HashSet[S]
    
    partialMembers*: seq[Node] #Fields from partial decls
    mixinMembers*: seq[Node]
    includes*: HashSet[S]
  
  DepsFinder* = ref object
    deps*: Table[string, DeclDeps[string]]
    skipUndeclared*: bool

proc init*(_: type DepsFinder, skipUndeclared: bool = true): DepsFinder =
  DepsFinder(
    deps: initTable[string, DeclDeps[string]](),
    skipUndeclared: skipUndeclared
  )

using self: DepsFinder

proc tryInitDeps(self; ident: Node)=
  assert ident.kind == Ident
  let s = ident.strVal
  if s notin self.deps:
    self.deps[s] = DeclDeps[string].default

proc countDeps*(self; s: string; countTable: var CountTable[string]) =
  # quite expensive operation, maybe better to use recursion ?
  countTable.inc s
  var deps = @[self.deps[s]]
  var globalUsed = HashSet[string].default
  while deps.len > 0:
    let i = deps.pop()
    var used = i.includes + i.usedTypes
    if i.inheritance.isSome:
      used.incl i.inheritance.get()
    
    used = used - globalUsed
    globalUsed.incl used

    while used.len > 0:
      var nextDecl = used.pop()
      if nextDecl in self.deps:
        deps.add self.deps[nextDecl]
      elif not self.skipUndeclared:
        raise newException(CatchableError, "Identifier " & nextDecl & " not found")

      countTable.inc s


proc setInheritance(deps: var DeclDeps[string], inheritance: Node)=
  deps.inheritance =
    if inheritance.kind == Empty:
      none(string)
    else:
      some(inheritance.strVal)

proc updateUsedTypes(node: Node, deps: var DeclDeps[string]) =
  # deps.usedTypes[]
  case node.kind:
    of Operation:
      let op =
        if node.inner.kind == RegularOperation:
          node.inner
        else:
          node.inner[0]
      
      assert op.kind == RegularOperation
      updateUsedTypes(op[1], deps)
      updateUsedTypes(op[2], deps)

    of ConstStmt:
      updateUsedTypes(node.inner, deps)
    
    of Attribute:
      updateUsedTypes(node[1], deps)
    
    of Readonly:
      if (let attribute = node.inner; attribute).kind == Attribute:
        updateUsedTypes(attribute, deps)
    
    of Setlike:
      updateUsedTypes(node[1], deps)

    of Maplike:
      updateUsedTypes(node[0], deps)
      updateUsedTypes(node[1], deps)
    
    of ArgumentList:
      for i in node.sons:
        updateUsedTypes(i, deps)

    of Argument:
      updateUsedTypes(node.inner[0], deps)
    
    of IdentDefs:
      updateUsedTypes(node[1], deps)

    of Type:
      if (let i = node.inner; i).kind == Ident and i.strVal notin keywordNames:
        deps.usedTypes.incl i.strVal
      else:
        for i in node.sons:
          updateUsedTypes(i, deps)

    of Required:
      updateUsedTypes(node.inner, deps)
    
    of Union:
      for i in node.sons:
        updateUsedTypes(i, deps)

    else:
      discard


proc findInterfaceLikeDeps(self; node: Node, fromPartial = false)=
  assert node.kind in {Interface, Dictionary, Namespace, Mixin}
  self.tryInitDeps(node[0])
  
  if node.kind in {Interface, Dictionary} and not fromPartial:
    self.deps[node[0].strVal].setInheritance(node[1])
  
  let startIdx =
    if node.kind in {Interface, Dictionary}:
      2
    else:
      1

  for i in node.sons[startIdx..^1]:
    updateUsedTypes(i[0], self.deps[node[0].strVal])

proc findDeps*(self; node: Node)=
  case node.kind:
    of Interface, Dictionary, Namespace:
      findInterfaceLikeDeps(self, node)
    of Mixin:
      findInterfaceLikeDeps(self, node)
      self.deps[node[0].strVal].mixinMembers &= node.sons[1..^1]
    of Typedef:
      self.tryInitDeps(node.name)
      updateUsedTypes(node[1], self.deps[node[0].strVal])
    of Enum:
      self.tryInitDeps(node.name)
    of Includes:
      # node[1] must be mixin
      self.tryInitDeps(node[0])
      self.deps[node[0].strVal].includes.incl node[1].strVal
    of Partial:
      # print self.deps[node.inner[0].strVal]
      findInterfaceLikeDeps(self, node.inner, true)
      # print self.deps[node.inner[0].strVal]

      let membersStartIdx =
        if node.inner.kind in {Interface, Dictionary}:
          2
        else:
          1

      self.deps[node.inner[0].strVal].partialMembers &=
        node.inner.sons[membersStartIdx..^1]
      # print self.deps[node.inner[0].strVal]
    else:
      discard

when isMainModule:
  import lexer
  import parser
  import deques, sequtils

  var t = tokenize"""
  interface mixin GPUObjectBase {
    attribute USVString label;
  };

  partial interface mixin GPUObjectBase {
    attribute USVString labelavd;
  };


  interface Base {
    attribute USVString test;
  };

  interface mixin GPUObjectBase2 {
    attribute USVString labeled_test;
  };

  interface GPUObjectBaseLol: Base {
    attribute USVString labelab;
  };

  partial interface GPUObjectBaseLol {
    attribute USVString PI;
  };

  GPUObjectBaseLol includes GPUObjectBase;
  GPUObjectBaseLol includes GPUObjectBase2;

  """
  t = tokenize"""
  typedef unsigned long GPUBufferDynamicOffset;
  """
  
  var c = parseCode(t).stack.toSeq
  echo t.len
  var finder = DepsFinder(
    deps: initTable[string, DeclDeps[string]]()
  )
  for i in c:
    finder.findDeps(i)
  
  var ct = initCountTable[string]()
  for i in finder.deps.keys:
    finder.countDeps(i, ct)
  echo ct
