import unode
import ast
import translate_types_dsl

import sequtils
import strformat
import sets
import sugar
import std/[with, tables, options]
import deps
import "$nim"/compiler/renderer
import object_signatures
from std/algorithm import SortOrder

type
  OptionalAttributePolicy* {.pure.} = enum
    MakeNoVariardic
    UseDefaultVal
    GenDeferredProcs

  ConstructorPolicy* {.pure.} = enum
    InitTypedesc
    InitName
    NewName
  
  Feature* {.pure.} = enum
    NamespaceJsFieldBinding
    InterfaceJsFieldBinding

    MethodCallSyntax

    ImportJsToImportCpp    
    JsRootToRootObj

    ObjConstrRequired
    ReadonlyAttributes

  OnIdentProc* = (NimUNode, bool) -> NimUNode

  TranslatorSettings* = object
    onIdent*: OnIdentProc

    optionalAttributePolicy*: OptionalAttributePolicy
    constructorPolicy*: ConstructorPolicy
    
    features*: set[Feature]
    exportCode*: bool
  
  Translator* = ref object
    settings*: TranslatorSettings
    imports*: HashSet[string]

    symCache: SymCache # webidl syms
    deps: Table[string, DeclDeps[string]]

    # typeMappings: seq[]
    nimTypes: seq[string]#=
  
  TranslatedDeclAssembly* = object
    decl: NimUNode
    declGenerated*: NimUNode
    declFields*: seq[NimUNode]
    
    bindRoutines*: seq[NimUNode]
    bindLets*    : seq[NimUNode] = @[]
  
  VariardicArg = tuple[
    val: NimUNode,
    isVariardic: bool
  ]

  OperationTranslationCtx* = object
    args: seq[VariardicArg]
  
  SymCache = object
    idTable: Table[int, Node]
    nextId: int

    symNames: Table[string, Sym]

  Sym = ref object
    id: int
    name: string
    kind: NodeKind
    # ast: Node

proc assemble*(decls: openArray[TranslatedDeclAssembly], imports: HashSet[string]): NimUNode =
  let typeSect = unode(unkTypeSection)
  let letSect = unode(unkLetSection)
  var routines = seq[NimUNode].default
  var imports = unode(unkImportStmt).add imports.mapIt(ident(it))

  for i in decls:
    typeSect.add i.declGenerated

    for j in i.bindLets:
      letSect.add j
    
    for j in i.bindRoutines:
      routines.add j
  
  result = unode(unkStmtList)
  with result:
    addIfNotEmpty imports
    addIfNotEmpty typeSect
    addIfNotEmpty letSect
    addIfNotEmpty routines

proc getAst*(cache: SymCache, s: Sym): Node =
  cache.idTable[s.id]

proc newSym*(self: Translator, name: string = ""; assembly = Node(kind: Empty)): Sym =
  result = Sym(
    id: self.symCache.nextId,
    name: name,
    kind: assembly.kind
    # ast: ast
  )
  self.symCache.symNames[name] = result
  self.symCache.idTable[result.id] = assembly

  inc self.symCache.nextId

proc findSym*(self: Translator; name: string): Sym =
  self.symCache.symNames[name]

proc tryFindSym*(self: Translator; name: string): Option[Sym] =
  if name in self.symCache.symNames:
    some(self.findSym(name))
  else:
    none(Sym)

proc findSym*(self: Translator; name: Node): Sym =
  self.findSym(name.strVal)

proc tryFindSym*(self: Translator; name: Node): Option[Sym] =
  self.tryFindSym(name.strVal)

translateTypesDsl nimTypes:
  undefined -> void

  (ByteString, DOMString, USVString) -> cstring
  boolean -> bool

  byte -> int8
  short -> int16
  long -> int32#ident
  (long long) -> int64#idents

  octet -> byte
  (unsigned short) -> uint16
  (unsigned long) -> uint32#{usigned, short}, uint32
  (unsigned long long) -> uint64

  float -> float32
  double -> float64

  Int8Array  -> seq[int8]
  Int16Array -> seq[int16]
  Int32Array -> seq[int32]
  BigInt64Array -> seq[int64]

  (Uint8Array, Uint8ClampedArray) -> seq[byte]
  Uint16Array -> seq[uint16]
  Uint32Array -> seq[uint32]
  Float32Array -> seq[float32]
  Float64Array -> seq[float64]
  BigUint64Array -> seq[uint64]

  ArrayBuffer:
    #`import` pkg/jsutils
    # ? maybe better to make switch
    `import` std/private/jsutils {.all.}
    -> ArrayBuffer

  sequence[_] -> seq[_]
  Promise[_]:
    `import` std/asyncjs
    -> Future[_]

  bigint:
    `import` std/jsbigints
    -> JsBigInt
  
  undefined:
    `import` std/jsffi
    -> JsObject # we can't use undefined
  
  object:
    `import` std/jsffi
    -> JsObject
  
  any:
    `import` std/jsffi
    -> JsObject

  record[_]:
    #not tested with ffi
    `import` std/jsffi
    -> JsObject
  
  EventTarget:
    `import` std/dom

const
  webidlNimIdents = [
    "void", "string", "bool", "int8", "int16", "int32", "int64",
    "byte", "uint16", "uint32", "uint64", "float32", "float64", "cstring",
    "auto"
  ]
  sep = ", "

proc genImportJsMethodPattern(argsCnt: int, methodName = "$1"): string =
  var args = newStringOfCap(sep.len * (argsCnt-2))
  if argsCnt - 2 > 0:
    args.add "#"
  for i in 1..<(argsCnt - 2):
    args.add sep
    args.add "#"

  "#." & methodName & '(' & args & ')'

using
  self: Translator
  assembly: var TranslatedDeclAssembly

proc jsRoot(self): auto =
  if JsRootToRootObj in self.settings.features:
    ident"RootObj"
  else:
    ident"JsRoot"

proc importJs(self): auto =
  if ImportJsToImportCpp in self.settings.features:
    ident"importcpp"
  else:
    ident"importjs"

proc toNimType*(self; n: Node): auto =
  let mapping = nimTypes.mapping
  mapping(n, self.imports)

proc translateIdentImpl(self; node: Node, capitalize: bool): auto =
  assert node.kind in {Ident, Empty}
  if node.isEmpty:
    return empty()

  self.settings.onIdent(
    ident node.strVal,
    capitalize
  )

proc translateIdent(self; node: Node): auto =
  translateIdentImpl(self, node, false)

proc translateDeclIdent(self; node: Node): auto =
  translateIdentImpl(self, node, true)

proc translateType*(self; node: Node, typeDefKind: NimUNodeKind = unkEmpty): auto =
  assert node.kind == Type

  var nimType = self.toNimType(node)

  if node.inner.kind != Ident or nimType.kind != unkIdent or nimType.strVal in webidlNimIdents:
    return nimType

  tryRemoveExportMarker self.settings.onIdent(
    nimType,
    true
  )

proc translateEmpty(node: Node): auto =
  assert node.kind == Empty
  empty()

proc translateIntLit(node: Node): auto =
  assert node.kind == IntLit
  intLit(node.intVal)

proc translateStrLit(node: Node): auto =
  assert node.kind == StrLit
  echo node.strVal
  strLit(node.strVal)

proc translateFloatLit(node: Node): auto =
  assert node.kind == FloatLit
  floatLit(node.floatVal)

proc translateBoolLit(node: Node): auto =
  assert node.kind == BoolLit
  
  case node.boolVal:
    of true: ident"true"
    of false: ident"false"

proc translateVal(self; node: Node): auto =
  assert node.kind in {Empty, IntLit, FloatLit, BoolLit, StrLit}
  case node.kind:
    of Empty:
      node.translateEmpty()
    of IntLit:
      node.translateIntLit()
    of FloatLit:
      node.translateFloatLit()
    of BoolLit:
      node.translateBoolLit()
    of StrLit:
      node.translateStrLit()
    else:
      raise newException(CatchableError, "Invalid node for val")



proc translateIdentDefs(self; node: Node): auto =
  assert node.kind == IdentDefs
  const anyStr = ["ByteString", "DOMString", "USVString"]

  let
    name    = node[0]
    t       = node[1]
    default = node[2]
  
  unode(unkIdentDefs).add(
    self.translateIdent name,
    self.translateType t,
    if default.kind == StrLit and t.inner.kind == Ident and t.inner.strVal notin anyStr:
      #? maybe need to raise exception if t is not enum ?
      tryRemoveExportMarker:
        self.translateDeclIdent Node(kind: Ident, strVal: default.strVal)
    elif default.kind in {IntLit, Ident} and t.inner.kind == Ident and t.inner.strVal notin anyStr:
      #? maybe need to raise exception if t is not distinct ?
      unode(unkCall).add(
        self.translateType t,
        self.translateVal default
      )
    else:
      self.translateVal default
  )

proc translateAttribute(self; node: Node): auto =
  assert node.kind == Attribute

  let
    name    = node[0]
    t       = node[1]
  
  var identDefsName = self.translateIdent name

  unode(unkIdentDefs).add(
    identDefsName,
    self.translateType t,
    empty()
  )

using opCtx: var OperationTranslationCtx

proc translateSimpleArgument(self, opCtx; node: Node) =
  assert node.kind == SimpleArgument

  var result = self.translateIdentDefs node[0]
  result[0] = tryRemoveExportMarker result[0]

  if node[1].kind == Ellipsis:
    result[1] =
      unode(unkBracketExpr)
      .add(ident"varargs")
      .add(result[1])
  
  opCtx.args.add (result, false)

# type IdentDefsVariants = object
proc translateOptionalArgument(self, opCtx;  node: Node) =
  assert node.kind == OptionalArgument
  let identDefs = node[0]
  
  
  var result = (self.translateIdentDefs identDefs, false).VariardicArg
  result[0][0] = tryRemoveExportMarker result[0][0]

  if identDefs[2].isEmpty:
    case self.settings.optionalAttributePolicy:
      of MakeNoVariardic:
        discard

      of UseDefaultVal:
        result[0][2] =
          unode(unkDotExpr)
          .add(self.translateType(identDefs[1], unkEmpty))
          .add(ident"default")

      of GenDeferredProcs:
        result.isVariardic = true  
  
  opCtx.args.add result

proc translateArgument(self, opCtx; node: Node) =
  assert node.kind == Argument
  case node[0].kind:
    of SimpleArgument:
      self.translateSimpleArgument(opCtx, node[0])
    of OptionalArgument:
      self.translateOptionalArgument(opCtx, node[0])
    else:
      raise newException(CatchableError, "Invalid argument")

proc translateArgumentList(self; node: Node): auto =
  # assert node.kind == ArgumentList
  var ctx: OperationTranslationCtx
  for i in node.sons:
    self.translateArgument(ctx, i)
  
  var argLists: seq[seq[NimUNode]] = @[@[]]
  for i in ctx.args:
    # [a, b, c], [a, b], [a]

    if i.isVariardic:
      argLists.add argLists[0]
    argLists[0].add i[0]

  argLists

# import print

proc translateRegularOperation*(self, assembly; node: Node) =
  assert node.kind == RegularOperation
  let argLists = self.translateArgumentList node[2]

  for argList in argLists:
    assembly.bindRoutines.add genRoutine(
        name = self.translateIdent node[0],
        returnType = self.translateType node[1],
        # pragmas = pragmas [pragma ident"importc"],
        params = argList
    )

proc translateOperation*(self, assembly; node: Node) =
  assert node.kind == Operation
  case node[0].kind:
    of RegularOperation:
      self.translateRegularOperation(assembly, node[0])
    else:
      raise newException(CatchableError, "Invalid operation")

proc setMethodBase*(m, base: NimUNode): auto =
  var params = unode(unkFormalParams)
  var p = m
  with params:
    add p[3][0]
    add base
    add p[3].sons[1..^1]

  p[3] = params
  p

proc translateNamespaceMember*(self, assembly; node: Node) =
  assert node.kind == NamespaceMember
  
  if NamespaceJsFieldBinding in self.settings.features:
    # firstly trying to translate field as js
    if node[0].kind == ConstStmt:
      var identDefs = self.translateIdentDefs node[0][0]
      identDefs[0] = identDefs[0].withPragma:
        pragma unode(unkExprColonExpr).add(
          ident "importc",
          strLit node[0][0][0].strVal
        )
          
      identDefs[2] = empty()
      assembly.declFields.add identDefs
      return

    elif node[0].kind == Operation and not node[0].isVariardic and MethodCallSyntax notin self.settings.features:
      self.translateOperation(assembly, node[0])
      var op = assembly.bindRoutines.pop()
      op[0] = empty() # clear name if generated
      op[4] = empty() # clear pragmas if generated

      var name = self.translateIdent(node[0].name)
      name = name.withPragma pragma unode(unkExprColonExpr).add(
        ident "importc",
        strLit node[0].name.strVal
      )
      
      assembly.declFields.add unode(unkIdentDefs).add(
        name,
        op,
        empty()
      )
      return

  let selfNode =
    unode(unkIdentDefs).add(
      ident"_",
      unode(unkBracketExpr).add(
        ident"typedesc",
        assembly.decl
      ),
      empty()
    )
  
  case node[0].kind:
    of Operation:
      var currentAssembly: TranslatedDeclAssembly
      self.translateOperation(currentAssembly, node[0])
      for procDef in currentAssembly.bindRoutines:
        #TODO: method call syntax fix importjs
        var fixedProcDef = procDef.setMethodBase(selfNode)
        if MethodCallSyntax in self.settings.features:
          fixedProcDef[4] = fixedProcDef[4].withPragma pragma unode(unkExprColonExpr).add(
            self.importJs,
            strLit(
              if node.name.strVal == fixedProcDef[0].tryRemoveExportMarker.strVal:
                genImportJsMethodPattern(procDef[3].sons.len)
              else:
                genImportJsMethodPattern(procDef[3].sons.len, node.name.strVal)
            )
          )
        else:
          fixedProcDef[4] = fixedProcDef[4].withPragma pragma ident"importc"
        
        assembly.bindRoutines.add fixedProcDef

    of ConstStmt:
      var identDefs = node[0][0]

      assembly.bindRoutines.add genRoutine(
        name = self.translateIdent identDefs[0],
        returnType = self.translateType identDefs[1],
        params = [selfNode],
        body = self.translateVal identDefs[2],
        routineType = unkTemplateDef
      )

    of Readonly:
      #TODO: maybe fix readonly ?
      assert node[0][0].kind == Attribute
      var attribute = node[0][0]

      assembly.bindRoutines.add genRoutine(
        name = self.translateIdent attribute[0],
        returnType = self.translateType attribute[1],
        params = [selfNode],
        routineType = unkProcDef,
        # pragmas = pragmas [pragma ident"importc"],
      )

    else:
      raise newException(CatchableError, "Invalid namespace member")

proc translateNamespace*(self; node: Node): TranslatedDeclAssembly =
  # var result = TranslatedDeclAssembly.default
  let t = self.translateDeclIdent(node[0])

  if NamespaceJsFieldBinding in self.settings.features:
    let jsTypeName = ident tryRemoveExportMarker(t).strVal & "Impl"
    result.decl = jsTypeName

    for i in node.sons[1..^1]:
      self.translateNamespaceMember(result, i)

    let jsType = genAlias(
      jsTypeName,
      unode(unkRefTy).add unode(unkObjectTy).add(
        empty(),
        unode(unkOfInherit).add(self.jsRoot),
        unode(unkRecList).add(result.declFields)
      )
    )
    result.declGenerated = jsType
    result.bindLets.add:
      unode(unkIdentDefs).add(
        t.withPragma pragma(
          ident"importc",
          ident"nodecl"
        ),
        jsTypeName,
        empty(),  
      )

  else:
    result.decl = t

    for i in node.sons[1..^1]:
      self.translateNamespaceMember(result, i)
    
    result.declGenerated = genDistinct(
      t,
      ident"void"
    )

# import ast_repr

proc translatePartialDictionary(self; node: Node): TranslatedDeclAssembly =
  # !Note: dictionary and partial dictionary is same
  # ! (only difference is inference)

  assert:
    node.kind == Dictionary# and
    # node[1].isEmpty # partial dictionary not support inference
  
  for i in node.sons[2..^1]:
    let n = i.inner
    var fieldPragmas = pragma(ident"importc")
    if (
      ObjConstrRequired in self.settings.features and
      n.kind == Required
    ): fieldPragmas.add(ident"requiresInit")
        
    var identDefs = self.translateIdentDefs(
      n.skipNodes({Required})
    )
    identDefs[0] = identDefs[0].withPragma fieldPragmas
    result.declFields.add identDefs

  result.decl = self.translateDeclIdent(node[0])
  result.declGenerated = genAlias(
    result.decl,
    unode(unkRefTy).add unode(unkObjectTy).add(
      empty(),
      unode(unkOfInherit).add(self.jsRoot),
      unode(unkRecList).add(result.declFields)
    )
  )

proc translatePartialInterface*(self; node: Node): TranslatedDeclAssembly =
  result.decl = self.translateDeclIdent(node[0])
  
  template makeField(attribute, node; hidden = false) =
    var field = self.translateAttribute(attribute)
    assert field.kind == unkIdentDefs
    # TODO: make hidden works >_<
    # if hidden: field[0] = tryRemoveExportMarker field[0]
    field[0] = field[0].withPragma:
      pragma unode(unkExprColonExpr).add(
        ident "importc",
        strLit node[0].strVal
      )
    result.declFields.add field
  
  template makeField(attribute; hidden = false) =
    makeField(attribute, attribute, hidden)
  
  template genAttribute(n) =
    var attribute = n.skipNodes({Readonly})
    if ReadonlyAttributes in self.settings.features and n.kind == Readonly:
      # type T = ref object of JsRoot
      #   attrHidden: TT
      # proc attr*(self: T) = self.attrHidden
      assert attribute[0].kind == Ident

      let procName = self.translateIdent attribute[0]
      attribute.sons[0].strVal =
        attribute[0].strVal & "_hidden"

      makeField(attribute, n.inner, true)
      result.bindRoutines.add genRoutine(
        name = procName,
        returnType = tryRemoveExportMarker(
          self.translateType attribute[1]
        ),
        params = [selfNode],
        body = unode(unkDotExpr).add(
          ident"self",
          tryRemoveExportMarker self.translateIdent(attribute[0]),
        ),            
        routineType = unkProcDef
      )
    else: makeField(attribute)
  
  template `*`(n: NimUNode): NimUNode =
    if self.settings.exportCode:
      unode(unkPostfix).add(ident"*", n)
    else: n

  let selfTypedescNode =
    unode(unkIdentDefs).add(
      ident"_",
      unode(unkBracketExpr).add(
        ident"typedesc",
        tryRemoveExportMarker result.decl
      ),
      empty()
  )
  let selfNode =
    unode(unkIdentDefs).add(
      ident"self",
      tryRemoveExportMarker result.decl,
      empty()
  )
  
  for i in node.sons[2..^1]:
    let n = i.inner
    case n.kind:
      of ConstStmt:
        #TODO: add js way
        var identDefs = n.inner

        result.bindRoutines.add genRoutine(
          name = self.translateIdent identDefs[0],
          returnType = self.translateType identDefs[1],
          params = [selfTypedescNode],
          body = self.translateVal identDefs[2],
          routineType = unkTemplateDef
        )

      of Readonly:
        case n.inner.kind:
          of Setlike:
            for i in readonlySetlike(
              self.settings.exportCode,
              tryRemoveExportMarker result.decl, 
              self.translateType n.inner[1]
            ): result.bindRoutines.add i


          of Maplike:
            for i in readonlyMaplike(
              self.settings.exportCode,
              tryRemoveExportMarker result.decl, 
              self.translateType n.inner[1]
            ): result.bindRoutines.add i

          of Attribute:
            genAttribute(n)
          else:
            raise newException(CatchableError, "Invalid readonly")
      
      of Attribute: genAttribute(n)

      of Operation:
        var currentAssembly: TranslatedDeclAssembly
        self.translateOperation(currentAssembly, n)
        for procDef in currentAssembly.bindRoutines:
          #TODO: method call syntax fix importjs
          var fixedProcDef = procDef.setMethodBase(selfNode)
          if MethodCallSyntax in self.settings.features:
            fixedProcDef[4] = fixedProcDef[4].withPragma pragma unode(unkExprColonExpr).add(
              self.importJs,
              strLit(
                if n.name.strVal == fixedProcDef[0].tryRemoveExportMarker.skipNodes({unkAccQuoted}).strVal:
                  genImportJsMethodPattern(procDef[3].sons.len)
                else:
                  genImportJsMethodPattern(procDef[3].sons.len, n.name.strVal)
              )
            )
          else:
            fixedProcDef[4] = fixedProcDef[4].withPragma pragma ident"importc"
          
          result.bindRoutines.add fixedProcDef
      
      of Setlike:
        for i in setlike(
          self.settings.exportCode,
          tryRemoveExportMarker result.decl, 
          self.translateType n[1]
        ): result.bindRoutines.add i

      of Maplike:
        for i in maplike(
          self.settings.exportCode,
          tryRemoveExportMarker result.decl,
          self.translateType n.inner[1]
        ): result.bindRoutines.add i

      of Constructor:
        # we maybe in not partial interface
        for argList in self.translateArgumentList(n.inner):
          let pragma = pragma unode(unkExprColonExpr).add(
            self.importJs,
            strLit do:
              "(new " & node[0].strVal & "(@))"
          )
          result.bindRoutines.add:
            case self.settings.constructorPolicy:
              of InitTypedesc:
                genRoutine(
                  name = *ident"init",
                  returnType = tryRemoveExportMarker result.decl,
                  pragmas = pragma,
                  params = selfTypedescNode & argList
                )
              of InitName, NewName:
                genRoutine(
                  name = *ident(
                    (
                    if self.settings.constructorPolicy == InitName:
                      "init"
                    else:
                      "new"
                    ) & (tryRemoveExportMarker result.decl).strVal
                  ),
                  returnType = tryRemoveExportMarker result.decl,
                  pragmas = pragma,
                  params = selfTypedescNode & argList
                )

      of Stringifier:
        if n.sons.len > 0 and n.inner.kind in {Attribute, Readonly}:
          genAttribute(n.inner)
          result.bindRoutines.add genRoutine(
            name = *unode(unkAccQuoted).add(ident"$"),
            returnType = ident"string",
            params = [selfNode],
            body = unode(unkPrefix).add(
              ident"$",
              unode(unkDotExpr).add(
                ident"self", 
                tryRemoveExportMarker(
                  self.translateIdent n.inner.skipNodes({Readonly})[0]
                )
              )
            )
          )
        else:
          result.bindRoutines.add genRoutine(
            name = *unode(unkAccQuoted).add(ident"$"),
            returnType = ident"string",
            params = [selfNode],
            pragmas = pragma unode(unkExprColonExpr).add(
              self.importJs,
              strLit"#.toString()"
            )
          )
      of Static:
        case n.inner.kind:
          of RegularOperation:
            var tmpAssembly = TranslatedDeclAssembly.default
            self.translateRegularOperation(tmpAssembly, n.inner)
            for i in tmpAssembly.bindRoutines:
              var formalParams = unode(unkFormalParams)
              with formalParams:
                add i[3][0]
                add selfTypedescNode
                add i[3].sons[1..^1]

              var procDef = i
              procDef[3] = formalParams
              result.bindRoutines.add procDef
          of Readonly, Attribute:
            # Same as const, but not defined in webidl
            let attribute = n.inner.skipNodes({Readonly})
            assert attribute.kind == Attribute
            
            result.bindRoutines.add genRoutine(
              name = self.translateIdent attribute[0],
              returnType = self.translateType attribute[1],
              params = [selfTypedescNode],
              pragmas = pragma unode(unkExprColonExpr).add(
                self.importJs,
                strLit(node[0].strVal & "." & attribute[0].strVal)
              ),
              routineType = unkProcDef
            )
          else:
            raise newException(CatchableError, "Strange node of kind " & $n.inner.kind & " in static member")
      of Iterable:
        proc genIteratorImpl(
          methodName: string, 
          outType: NimUNode, 
          nimName =
            self.translateIdent Node(kind: Ident, strVal: methodName)
        ): auto =
          let
            methodNameS = strLit('.' & methodName & "()")
            selfName = selfNode[0]
          
          let body = ugenAst(methodNameS, outType, selfName):
            {.emit: ["var it = ", selfName, methodNameS].}
            var it {.importc, nodecl.}: JsObject
            while true:
              {.emit: "let next = it.next();".}
              let next {.importc, nodecl.}: JsObject
                
              if next.done.to(bool):
                break

              yield next.value.to(outType)
          
          
          genRoutine(
            name = nimName,
            returnType = outType,
            params = [selfNode],
            pragmas = pragma unode(unkExprColonExpr).add(
              self.importJs,
              strLit(node[0].strVal & "." & methodName & "()")
            ),
            body = body,
            routineType = unkIteratorDef
          )


        template genIterator(methodName: string, outType: NimUNode)=
          result.bindRoutines.add genIteratorImpl(methodName, outType)
        template genIterator(methodName: string, outType: NimUNode, nimName: NimUNode)=
          result.bindRoutines.add genIteratorImpl(methodName, outType, nimName)
        
        self.imports.incl "std/jsffi"
        var v = self.translateType n[1]
        genIterator("values", v)
        if n[0].kind != Empty:
          let k = self.translateType n[0]
          genIterator("keys", k)
          genIterator(
            "entries", 
            unode(unkTupleConstr).add(k, v),
            *ident"pairs"
          )
      
      else:
        discard
  
  let recList = unode(unkRecList).add(result.declFields)

  result.declGenerated = genAlias(
    result.decl,
    unode(unkRefTy).add unode(unkObjectTy).add(
      empty(),
      unode(unkOfInherit).add(self.jsRoot),
      recList
    )
  )

proc translateInterface*(self; node: Node): TranslatedDeclAssembly =
  assert node.kind == Interface
  var n = node
  let deps = self.deps[node.name.strVal]

  if deps.inheritance.isSome:
    let inheritanceSym = self.findSym(deps.inheritance.get())
    assert inheritanceSym.kind == Interface
    var node = self.symCache.getAst(inheritanceSym)
    
    # let inheritanceDeps 
    for i in node.sons[2..^1]:# & self.deps[deps.inheritance.get]:
      n.add i
  
  for i in deps.partialMembers:
    n.add i

  for i in deps.includes:
    let mixinDeps = self.deps[i]
    for j in (mixinDeps.mixinMembers & mixinDeps.partialMembers):
      n.add j
  
  self.translatePartialInterface(n)

proc translateTypedef(self; node: Node): TranslatedDeclAssembly =
  result.decl = self.translateDeclIdent node[0]
  result.declGenerated = genDistinct(
    result.decl,
    self.translateType node[1]
  )

proc translateEnum*(self; node: Node): TranslatedDeclAssembly =
  result.decl = self.translateDeclIdent node[0]
  result.declFields = node.sons[1..^1].mapIt:
    tryRemoveExportMarker(
      self.translateDeclIdent Node(kind: Ident, strVal: it.strVal)
    )
  
  result.declGenerated = genAlias(
    result.decl,
    unode(unkEnumTy).add unode(unkStmtList).add(result.declFields)
  )

# import ast_repr
proc translate*(self; node: Node): TranslatedDeclAssembly =
  result = case node.kind:
    of Interface:
      self.translateInterface(node)
    of Dictionary:
      self.translatePartialDictionary(node)
    of Namespace:
      self.translateNamespace(node)
    of Typedef:
      self.translateTypedef(node)
    of Enum:
      self.translateEnum(node)
    else:
      raise newException(CatchableError, "Invalid decl:  " & $node)

  discard self.newSym(node.name.strVal, node)

proc getInheritanceOrder*(nodes: seq[Node]; finder: var DepsFinder): seq[string] =
  for i in nodes:
    finder.findDeps(i)
  
  var ct = initCountTable[string]()
  for i in finder.deps.keys:
    finder.countDeps(i, ct)
  ct.sort(Ascending)
  ct.keys.toSeq()

proc genDeclTable(nodes: seq[Node]): Table[string, Node] =
  for i in nodes:
    if i.kind in {Includes, Partial}:
      continue
    if i.kind == Empty:
      # dirty hack. Empty must be not in nodes
      continue

    result[i.name.strVal] = i

proc translate*(self; nodes: seq[Node], allowUndeclared = false): seq[TranslatedDeclAssembly] =
  var table = nodes.genDeclTable()
  var finder = DepsFinder.init(allowUndeclared)
  let order = getInheritanceOrder(nodes, finder)
  self.deps = finder.deps
  for i in order:
    if table[i].kind in {Includes, Mixin, Partial}:
      # no need to translate because it only changes exists defs
      continue
    result.add self.translate(table[i])
