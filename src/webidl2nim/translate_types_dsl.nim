import std/[options, tables, sets, sequtils]
import ast
from ast_gen import generic, empty
import macros

#! this file need code refactor

proc webidlIdent(s: string): Node =
  Node(kind: Ident, strVal: s)

type
  NimReplacementAction* = enum
    None
    Import

  NimReplacement* = object
    t: NimNode
    case action: NimReplacementAction
      of None:
        discard
      of Import:
        imports: HashSet[string]
  
  GenericArgKind* {.pure.} = enum
    Regular
    Sequence
  
  GenericArg = object
    case kind: GenericArgKind
      of Regular:
        name*: string
      of Sequence:
        discard

  GenericArgs = seq[GenericArg]
  GenericArgsChangeDesc = tuple[before, after: GenericArgs]

  Index = object
    case backwards: bool
      of true:
        backwardsIndex: BackwardsIndex
      of false:
        intIndex: int

proc index(i: BackwardsIndex): Index=
  Index(backwards: true, backwardsIndex: i)

proc index(i: int): Index=
  Index(backwards: false, intIndex: i)

proc inc(i: var Index)=
  case i.backwards:
    of true:
      dec i.backwardsIndex
    of false:
      inc i.intIndex

proc `$`(i: Index): string {.used.} =
  case i.backwards:
    of true:
      '^' & $i.backwardsIndex.int
    of false:
      $i.intIndex

proc getNode(i: Index): NimNode=
  case i.backwards:
    of true:
      prefix(i.backwardsIndex.int.newIntLitNode, "^")
    of false:
      newIntLitNode(i.intIndex)

proc `==`(l,r: GenericArg): bool=
  if l.kind != r.kind:
    return
  case l.kind:
    of Sequence:
      true
    of Regular:
      l.name == r.name


proc getIdents(t: NimNode): seq[NimNode]=
  if t.kind == nnkIdent:
    return @[t]
  
  t[0] & getIdents(t[1])

proc replacement(t: NimNode): NimReplacement=
  NimReplacement(action: None, t: t)

iterator applyGenericArgDesc(
  changeDesc: GenericArgsChangeDesc
  ): NimNode =
  var (argsDescIn, argsDescOut) = changeDesc
  var seqPos: tuple[start: int, stop: BackwardsIndex]#stop right
  
  for i, val in argsDescIn:
    if val.kind == Sequence:
      seqPos.start = i
      seqPos.stop = ^(argsDescIn.len - i)
      break

  var
    vars = initTable[GenericArg, Index]()
    i: Index = 0.index

  for val in argsDescIn:
    case val.kind:
      of Regular:
        vars[val] = i
      of Sequence:
        #skip sequence in index
        i = seqPos.stop.index

    inc i

  for val in argsDescOut:
    case val.kind
      of Regular:
        if val in vars:
          yield vars[val].getNode
      of Sequence:
        if seqPos.start == 0 and seqPos.stop.int == 1:
          yield newEmptyNode()
        else:
          yield infix(seqPos.start.index.getNode, "..", seqPos.stop.index.getNode)

proc translateGenericArgs(procName: NimNode, name: NimNode, changeDesc: GenericArgsChangeDesc): NimNode =
  assert name.kind == nnkIdent

  for i in applyGenericArgDesc(changeDesc):
    var curr =
      if i.kind == nnkEmpty:
        name
      else:
        newNimNode(nnkBracketExpr)
        .add(name)
        .add(i)
    
    curr =
      if i.kind in {nnkPrefix, nnkIntLit}:
        quote: @[`curr`.`procName`(imports)]
      else:
        quote do:
          `curr`.mapIt(it.`procName`(imports))
    
    if result.isNil:
      result = curr
    else:
      result = infix(result, "&", curr)




proc addGenericBranch(t, bGenericBase, aGenericBase, aGenericArgsIdx: NimNode): (NimNode, NimNode)=
  let
    args = macros.ident"args"
    ident = macros.ident"ident"

    cond = quote: `t`.kind == Generic and `t`.sons[0].strVal == `bGenericBase`
    body = quote:
      let `args` = `t`.sons[1..^1]
      unode(unkBracketExpr)
        .add(`aGenericBase`.`ident`)
        .add(`aGenericArgsIdx`)#.mapIt(it.strVal.ident))
        
  
  (cond, body)

proc addGenericToIdentBranch(t, bGenericBase, aName: NimNode): (NimNode, NimNode)=
  let
    ident = macros.ident"ident"

    cond = quote: `t`.kind == Generic and `t`.sons[0].strVal == `bGenericBase`
    body = quote: `ident`(`aName`)
        
  
  (cond, body)



proc addIdentBranch(t, bName, aName: NimNode): (NimNode, NimNode)=
  let
    ident = macros.ident"ident"

    cond = quote: `t`.kind == Ident and `t`.strVal == `bName`
    body = quote: `ident`(`aName`)

  (cond, body)

proc addIdentToGenericBranch(t, bName: NimNode, aGeneric: seq[NimNode]): (NimNode, NimNode)=
  let
    ident = macros.ident"ident"
    aGenericIdents = newNimNode(nnkBracket).add:
      aGeneric.mapIt(quote do: `ident`(`it`))

    cond = quote: `t`.kind == Ident and `t`.strVal == `bName`
    body = quote:
      unode(unkBracketExpr).add(`aGenericIdents`)
  (cond, body)


proc addIdentsBranch(t: NimNode, bIdentsStrs: openArray[NimNode], aName: NimNode): (NimNode, NimNode)=
  var signatureCond: NimNode
  for i, val in bIdentsStrs:
    let cond = quote:
      `t`.sons[`i`].strVal == `val`
    
    signatureCond =
      if signatureCond.isNil: cond
      else: infix(signatureCond, "and", cond)

  let
    ident = macros.ident"ident"

    identsLen = bIdentsStrs.len
    cond = quote: `t`.kind == Idents and `t`.sons.len == `identsLen` and `signatureCond`
    body = quote: `ident`(`aName`)

  (cond, body)

proc parseGenericArgs(args: openArray[NimNode]): GenericArgs=
  for i in args:
    assert i.kind in {nnkIdent, nnkIntLit}

    if i.strVal == "_":
      result.add GenericArg(kind: Sequence)
    else:
      result.add GenericArg(kind: Regular, name: i.repr)

proc translateTypesDslImpl(
  body: NimNode, 
  res: var seq[(Node, NimReplacement)],
  genericsArgChangeDescs: var Table[Node, GenericArgsChangeDesc]
  )=
  for i in body:
    case i.kind:
      of nnkInfix:
        assert i[0].strVal == "->"
        let
          webidlType = i[1]
          nimType    = i[2]
        
        case webidlType.kind:
          of nnkIdent, nnkObjectTy:
            var node = webidlIdent(webidlType.repr)
            # echo node
            res.add (node, nimType.replacement)
          
          of nnkTupleConstr:
            for i in webidlType:
              translateTypesDslImpl(newStmtList(infix(i, "->", nimType)), res, genericsArgChangeDescs)

          of nnkPar, nnkCommand:
            #! generic not supported
            var node =
              if webidlType.len > 0:
                Node(kind: Idents)
              else:
                Node(kind: Ident)

            for j in webidlType:
              if j.kind == nnkIdent:
                node.sons.add webidlIdent(j.strVal)
              else:
                node.sons.add getIdents(j).mapIt(webidlIdent(it.strVal))

            res.add (node, nimType.replacement)
          
          of nnkBracketExpr:
            var node = webidlIdent(webidlType[0].strVal).generic
            genericsArgChangeDescs[node] = (
              parseGenericArgs(webidlType[1..^1]),
              parseGenericArgs(nimType[1..^1])
            )

            res.add (node, nimType[0].replacement)
          
          else:
            discard

      of nnkCall:
        # Promise[_]:
        #   `import` std/asyncjs
        #   -> PromiseJs[_]

        var nimReplacement = NimReplacement(t: newEmptyNode())
        var webidlType = i[0]

        var webidlTypeNode =
          case webidlType.kind:
            of nnkIdent, nnkObjectTy:
              webidlIdent(webidlType.repr)
            of nnkBracketExpr:
              webidlIdent(webidlType[0].strVal).generic
            else:
              empty()
        
        if webidlTypeNode.kind == Generic:
          genericsArgChangeDescs[webidlTypeNode] = GenericArgsChangeDesc.default
          genericsArgChangeDescs[webidlTypeNode].before = parseGenericArgs(webidlType[1..^1])

        for field in i[1]:
          case field.kind:
            of nnkCommand:
              #import moduleName

              var op =
                if field[0].kind == nnkAccQuoted: field[0][0].strVal
                else: field[0].strVal

              assert op == "import"
              case op:
                of "import":
                  nimReplacement.action = Import
                  nimReplacement.imports = [field[1].repr].toHashSet

            of nnkPrefix:
              # -> NimType

              assert field[0].strVal == "->"
              var nimType = field[1]

              case nimType.kind:
                of nnkIdent:
                  nimReplacement.t = nimType
                of nnkBracketExpr:
                  # PromiseJs[_]
                  nimReplacement.t = nimType[0]


                  #! we need that webidl type is generic
                  genericsArgChangeDescs[webidlTypeNode].after = parseGenericArgs(nimType[1..^1])

                else:
                  raise newException(CatchableError, "Invalid out nim type")

            else:
              discard

        if nimReplacement.t.kind == nnkEmpty:
          nimReplacement.t = webidlType

        # imports.incl nimReplacement.imports
        res.add (webidlTypeNode, nimReplacement)
      else:
        discard

macro translateTypesDsl*(name: untyped, body: untyped): untyped=
  var 
    nodes: seq[(Node, NimReplacement)]
    changeDescs: Table[Node, GenericArgsChangeDesc]
  #NimReplacement t[]
  translateTypesDslImpl(body, nodes, changeDescs)
  # echo nodes
  # echo "imp: ", imports
  # echo changeDescs
  var
    procName = macros.ident(name.strVal & "Impl")
    t = macros.ident"t"
    tContainer = macros.ident"tContainer"
    ident = macros.ident"ident"
    nestList = macros.ident"nestList"
    imports = macros.ident"imports"

  let ifNode = newNimNode(nnkIfStmt)

  for i, (inNode, outNode) in nodes:
    var cond, body: NimNode

    case inNode.kind:
      of Ident:
        (cond, body) = 
          if outNode.t.kind == nnkBracketExpr:
            addIdentToGenericBranch(
              t,
              newStrLitNode(inNode.strVal), 
              outNode.t.mapIt(newStrLitNode it.strVal)
            )
          else:
            addIdentBranch(
              t,
              newStrLitNode(inNode.strVal), 
              newStrLitNode(outNode.t.strVal)
            )
      of Idents:
        (cond, body) = addIdentsBranch(
          t,
          inNode.sons.mapIt(newStrLitNode(it.strVal)), 
          newStrLitNode(outNode.t.strVal)
        )
      of Generic:
        (cond, body) =
          if changeDescs[inNode].after.len == 0:
            addGenericToIdentBranch(
              t,
              newStrLitNode(inNode.sons[0].strVal),
              newStrLitNode(outNode.t.strVal),
            )
          else:
            let
              args = translateGenericArgs(
                procName,
                macros.ident"args", 
                changeDescs[inNode],
              )

            addGenericBranch(
              t,
              newStrLitNode(inNode.sons[0].strVal),
              newStrLitNode(outNode.t.strVal),
              args
            )
      else:
        raise newException(CatchableError, "Unsupported node kind")
    
    # add import to imports
    if outNode.action == Import:
      if body.kind != nnkStmtList:
        body = newNimNode(nnkStmtList).add(body)
      
      var outType = body
      
      var importSeq = outNode.imports.toSeq
      body = newNimNode(nnkStmtList).add quote do:
        `imports`.incl `importSeq`.toHashSet
      body.add outType



    ifNode.add newNimNode(nnkElifBranch).add(cond, body)
  
  ifNode.add newNimNode(nnkElifBranch).add(
    quote do: `t`.kind == Ident,
    quote do: `ident`(`t`.strVal)
  )

  ifNode.add newNimNode(nnkElifBranch).add(
    quote do: `t`.kind == Union,
    quote do: `nestList`(
      `ident`("or"), 
      `t`.sons.mapIt(it.`procName`(`imports`)),
      unkInfix
    )
  )

  ifNode.add newNimNode(nnkElifBranch).add(
    quote do: `t`.kind == Generic,
    quote do:
      unode(unkBracketExpr)
        .add(`t`.sons[0].strVal.`ident`)
        .add(`t`.sons[1..^1].mapIt(it.`procName`(`imports`)))
  )

  ifNode.add newNimNode(nnkElse).add quote do: 
    raise newException(CatchableError, "Invalid webidl type")
  
  # var genProc = macros.ident("gen" & capitalizeAscii(name.strVal) & "Proc")

  var r = quote do:
    type `name`* = object
    
    proc `procName`(`tContainer`: Node, `imports`: var HashSet[string]): NimUNode =
      assert `tContainer`.kind == Type
      let `t` = `tContainer`.sons[0]
      `ifNode`
    
    proc mapping*(_: type `name`): auto =
      `procName`
    

  when defined(webidl2nim.debug):  
    echo r.repr
  r
