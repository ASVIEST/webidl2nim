import std/[sugar, sequtils]

type
  NimUNodeKind* = enum
    unkNone, unkEmpty, unkIdent, unkSym, unkType, unkCharLit, unkIntLit,
    unkInt8Lit, unkInt16Lit, unkInt32Lit, unkInt64Lit, unkUIntLit, unkUInt8Lit,
    unkUInt16Lit, unkUInt32Lit, unkUInt64Lit, unkFloatLit, unkFloat32Lit,
    unkFloat64Lit, unkFloat128Lit, unkStrLit, unkRStrLit, unkTripleStrLit,
    unkNilLit, unkComesFrom, unkDotCall, unkCommand, unkCall, unkCallStrLit,
    unkInfix, unkPrefix, unkPostfix, unkHiddenCallConv, unkExprEqExpr,
    unkExprColonExpr, unkIdentDefs, unkVarTuple, unkPar, unkObjConstr, unkCurly,
    unkCurlyExpr, unkBracket, unkBracketExpr, unkPragmaExpr, unkRange, unkDotExpr,
    unkCheckedFieldExpr, unkDerefExpr, unkIfExpr, unkElifExpr, unkElseExpr,
    unkLambda, unkDo, unkAccQuoted, unkTableConstr, unkBind, unkClosedSymChoice,
    unkOpenSymChoice, unkHiddenStdConv, unkHiddenSubConv, unkConv, unkCast,
    unkStaticExpr, unkAddr, unkHiddenAddr, unkHiddenDeref, unkObjDownConv,
    unkObjUpConv, unkChckRangeF, unkChckRange64, unkChckRange, unkStringToCString,
    unkCStringToString, unkAsgn, unkFastAsgn, unkGenericParams, unkFormalParams,
    unkOfInherit, unkImportAs, unkProcDef, unkMethodDef, unkConverterDef,
    unkMacroDef, unkTemplateDef, unkIteratorDef, unkOfBranch, unkElifBranch,
    unkExceptBranch, unkElse, unkAsmStmt, unkPragma, unkPragmaBlock, unkIfStmt,
    unkWhenStmt, unkForStmt, unkParForStmt, unkWhileStmt, unkCaseStmt,
    unkTypeSection, unkVarSection, unkLetSection, unkConstSection, unkConstDef,
    unkTypeDef, unkYieldStmt, unkDefer, unkTryStmt, unkFinally, unkRaiseStmt,
    unkReturnStmt, unkBreakStmt, unkContinueStmt, unkBlockStmt, unkStaticStmt,
    unkDiscardStmt, unkStmtList, unkImportStmt, unkImportExceptStmt,
    unkExportStmt, unkExportExceptStmt, unkFromStmt, unkIncludeStmt, unkBindStmt,
    unkMixinStmt, unkUsingStmt, unkCommentStmt, unkStmtListExpr, unkBlockExpr,
    unkStmtListType, unkBlockType, unkWith, unkWithout, unkTypeOfExpr,
    unkObjectTy, unkTupleTy, unkTupleClassTy, unkTypeClassTy, unkStaticTy,
    unkRecList, unkRecCase, unkRecWhen, unkRefTy, unkPtrTy, unkVarTy, unkConstTy,
    unkMutableTy, unkDistinctTy, unkProcTy, unkIteratorTy, unkSharedTy, unkEnumTy,
    unkEnumFieldDef, unkArgList, unkPattern, unkHiddenTryStmt, unkClosure,
    unkGotoState, unkState, unkBreakState, unkFuncDef, unkTupleConstr, unkError ## erroneous AST node
  
  NimUNode* = ref NimUNodeObj
  NimUNodeObj = object
    case kind*: NimUNodeKind
      of unkNone, unkEmpty, unkNilLit:
        discard
      of unkCharLit..unkUInt64Lit:
        intVal: BiggestInt
      of unkFloatLit..unkFloat64Lit:
        floatVal: BiggestFloat
      of unkStrLit..unkTripleStrLit, unkCommentStmt, unkIdent, unkSym:
        strVal*: string
      else:
        sons*: seq[NimUNode]

{.push inline.}

func len*(node: NimUNode): int = node.sons.len

func unode*(kind: NimUNodeKind): NimUNode=
  NimUNode(kind: kind)

proc add*(self, son: NimUNode): NimUNode {.discardable.} =
  self.sons.add(son)
  self

proc add*(father: NimUNode, children: varargs[NimUNode]): NimUNode {.discardable.} =
  father.sons.add(children)
  result = father

proc addIfNotEmpty*(self, son: NimUNode): NimUNode {.discardable.} =
  if son.kind == unkEmpty: return self
  self.add son

proc addIfNotEmpty*(self: NimUNode, sons: openArray[NimUNode]): NimUNode {.discardable.} =
  if self.kind == unkEmpty: return self
  self.add sons

func `[]`*(self: NimUNode, i: int): var NimUNode =
  self.sons[i]

func `[]=`*(self: var NimUNode, i: int, val: NimUNode) =
  self.sons[i] = val

func empty*(): NimUNode =
  NimUNode(kind: unkEmpty)

func ident*(s: string): NimUNode =
  NimUNode(kind: unkIdent, strVal: s)

func strLit*(s: string): NimUNode =
  NimUNode(kind: unkStrLit, strVal: s)

func intLit*(i: BiggestInt): NimUNode =
  NimUNode(kind: unkIntLit, intVal: i)

func floatLit*(f: BiggestFloat): NimUNode =
  NimUNode(kind: unkFloatLit, floatVal: f)

func makePublic*(n: NimUNode): NimUNode {.discardable.} =
  let n =
    unode(unkPostfix)
    .add(ident("*"))
    .add(n)
  n

import std/strutils

{.pop.}
func nep1Rename(name: string, capitalize: bool): string {.discardable, inline.} =
  result = name
  
  if result.all(x => x.isUpperAscii or x in {'_', '-'}):
    result = result.toLower

  if result[0] == '-':
    result = result[1..^1]

  # if capitalize:
  #   result = result.capitalizeAscii
  # else:
  #   result[0] = result[0].toLowerAscii

  var res: string
  for i in 0..<result.high:
    if result[i] in {'-', '_'}:
      result[i + 1] = result[i + 1].toUpperAscii
    else:
      res.add result[i]

  res.add result[^1]

  return res

{.push inline.}
func nep1Rename*(n: NimUNode, capitalize: static bool = true): NimUNode =
  ident nep1Rename(n.strVal, capitalize)
#   let n =
#     unode(unkPostfix)
#     .add(ident("*"))
#     .add(n)
#   n

func applyOn*(
  node: NimUNode, 
  cond: bool, 
  runableProc:
    proc (n: NimUNode): NimUNode {.inline.} |
    proc (n: NimUNode): NimUNode
  ): NimUNode =
  if cond:
    runableProc(node)
  else:
    node

func pragma*(body: varargs[NimUNode]): NimUNode =
  unode(unkPragma).add body

func stmtList*(): NimUNode=
  unode(unkStmtList)

func typeSection*(typeDefs: varargs[NimUNode]): NimUNode =
  unode(unkTypeSection).add(typeDefs)



const RoutineNodes* = {
  unkProcDef, unkFuncDef, unkMethodDef, unkDo, unkLambda,
  unkIteratorDef, unkTemplateDef, unkConverterDef, unkMacroDef
}

proc expectKind*(n: NimUNode; k: set[NimUNodeKind]) =
  ## Checks that `n` is of kind `k`. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check the AST that is passed to them.
  if n.kind notin k:
    echo "wtf"
    # error("Expected one of " & $k & ", got " & $n.kind, n)

proc withPragma*(n: NimUNode, pragma: NimUNode): NimUNode =
  unode(unkPragmaExpr).add(n, pragma)


proc tryRemoveExportMarker*(n: NimUNode): NimUNode =
  #TODO: make it with full tree visit via stack
  result = n
  if n.kind == unkPostfix and n[0].strVal == "*":
    result = n[1]

func genRoutine*(
  name = empty(),
  params: openArray[NimUNode] = [],
  body: NimUNode = empty(),
  returnType: NimUNode = empty(),
  routineType = unkProcDef,
  pragmas: NimUNode = empty()
): NimUNode =
  # if procType notin RoutineNodes:
  #   discard
  # pragmas.expectKind({unkEmpty, unkPragma})
  result = unode(routineType).add(
    name,
    empty(),
    empty(),
    if params.len > 0 or returnType.kind != unkEmpty:
      unode(unkFormalParams).add(returnType).add(params)
    else:
      empty(),

    pragmas,
    empty(),
    body)

    # error("Expected one of " & $RoutineNodes & ", got " & $procType)

func genAlias*(name, base: NimUNode): NimUNode =
  # var typeDefName = name
      
  unode(unkTypeDef).add(name, empty(), base)

func genDistinct*(name, base: NimUNode): NimUNode =
  # var typeDefName = name
  let
    distinctBody = unode(unkDistinctTy).add(base)
    typeDef   = unode(unkTypeDef).add(name, empty(), distinctBody)
      
  typeDef

func genEnum*(
  name: NimUNode, fields: openArray[NimUNode], 
  pure: bool = true
  ): NimUNode =
  let enumBody =
    unode(unkEnumTy)
    .add(empty())
    .add(fields)

  var typeDefArgs = [name, empty(), enumBody]
        
  if pure:
    typeDefArgs[0] = 
      unode(unkPragmaExpr)
      .add(typeDefArgs[0])
      .add(pragma ident("pure"))

  let
    typeDef   = add(unode(unkTypeDef), typeDefArgs)

  typeDef

proc nestList*(op: NimUNode; pack: openArray[NimUNode], kind: NimUNodeKind = unkCall): NimUNode =
  ## Nests the list `pack` into a tree of call expressions:
  ## `[a, b, c]` is transformed into `op(a, op(c, d))`.
  ## This is also known as fold expression.
  # if pack.len < 1:
  #   error("`nestList` expects a node with at least 1 child")
  result = pack[^1]
  for i in countdown(pack.len - 2, 0):
    result =
      unode(kind)
      .add(op)
      .add(pack[i], result)

proc isSimpleOrdinal*(node: NimUNode): bool =
  # ?maybe better run nim interpreter via nimscript
  assert node.kind == unkIdent
  if node.strVal in [
    "byte",
    "uint8",
    "int8",
    "int16",
    "uint16",
    "int32",
    "uint32",
    "int64",
    "uint64",
    "char",
    "cint",
    "cuint",
    "clong",
    "culong",
    "clonglong",
    "culonglong",
    "cchar",
    "cuchar"
  ]: true
  else: false

{.pop.}




import std/macros
import "$nim"/compiler/[ast, idents, lineinfos]

proc toNimNode*(node: NimUNode): NimNode =
  result = newNimNode NimNodeKind(node.kind.ord())

  case node.kind:
    of unkNone, unkEmpty, unkNilLit:
      discard

    of unkCharLit..unkUInt64Lit:
      result.intVal = node.intVal

    of unkFloatLit..unkFloat64Lit:
      result.floatVal = node.floatVal

    of unkStrLit..unkTripleStrLit, unkCommentStmt, unkIdent, unkSym:
      result.strVal = node.strVal
    
    else:
      for i in node.sons:
        result.add i.toNimNode()

var cache = newIdentCache()
import "$nim"/compiler/renderer

proc toPNode*(node: NimUNode): PNode =
  if node.kind == unkIdent:
    return newIdentNode(
      cache.getIdent(node.strVal), 
      unknownLineInfo
    )

  result = newNode TNodeKind(node.kind.ord())

  case node.kind:
    of unkNone, unkEmpty, unkNilLit:
      discard

    of unkCharLit..unkUInt64Lit:
      result.intVal = node.intVal

    of unkFloatLit..unkFloat64Lit:
      result.floatVal = node.floatVal

    of unkStrLit..unkTripleStrLit, unkCommentStmt, unkIdent, unkSym:
      result.strVal = node.strVal
    
    else:
      for i in node.sons:
        # echo i.toPNode, " :  ", i.kind
        result.add i.toPNode()
