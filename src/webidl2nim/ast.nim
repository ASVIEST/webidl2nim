type
  SpecialOperationKeyword* {.pure.} = enum
    Getter = "getter"
    Setter = "setter"
    Deleter = "deleter"

  NodeKind* {.pure.} = enum
    Empty
    Interface
    InterfaceMember
    Dictionary
    DictionaryMember
    Enum #Ident StrLit*
    Namespace
    NamespaceMember
    Mixin
    MixinMember

    Typedef
    Includes

    Ident
    Idents
    Generic
    Union

    IdentDefs#Ident    [Type]    StrLit | IntLit | FloatLit

    Type
    Partial
    Constructor
    Stringifier
    Static

    ConstStmt#IdentDefs
    Readonly
    Attribute
    Operation
    RegularOperation
    SpecialOperation
    Required

    Argument
    ArgumentList
    OptionalArgument
    SimpleArgument

    StrLit
    BoolLit
    IntLit
    FloatLit

    Ellipsis
    Special

    Iterable
    Maplike
    Setlike

    Inherit

    Async
    Callback

  Node* = object
    case kind*: NodeKind
      of Special:
        specOpKw*: SpecialOperationKeyword

      of Empty, Ellipsis:
        discard
      of FloatLit:
        floatVal*: float
      of IntLit:
        intVal*: int
      of BoolLit:
        boolVal*: bool
      of StrLit, Ident:
        strVal*: string
      else:
        sons*: seq[Node]

const
  InheritancedDecls* = {Interface, Dictionary}

proc cmp*(l, r: Node): bool=
  l.kind == r.kind and (
    case l.kind:
      of Special:
        l.specOpKw == r.specOpKw
      of Empty, Ellipsis:
        true
      of FloatLit:
        l.floatVal == r.floatVal
      of IntLit:
        l.intVal == r.intVal
      of BoolLit:
        l.boolVal == r.boolVal
      of StrLit, Ident:
        l.strVal == r.strVal
      else:
        if l.sons.len != r.sons.len:
          return false
        var res = true
        for i in 0..l.sons.high:
          res = res and cmp(l.sons[i], r.sons[i])
        res
  )

proc `==`*(l, r: Node): bool= cmp(l, r)

func isEmpty*(n: Node): bool=
  n.kind == Empty

func inner*(n: Node): Node=
  n.sons[0]

func `[]`*(n: Node, i: int): Node=
  n.sons[i]

func isVariardic*(n: Node): bool =
  case n.kind:
    of SimpleArgument:
      n[1].kind == Ellipsis
    of OptionalArgument:
      n.inner[2].isEmpty
    of Argument:
      n.inner.isVariardic
    of ArgumentList:
      var res = false
      for i in n.sons:
        res = res or i.isVariardic
      res
    of RegularOperation:
      n[2].isVariardic
    of SpecialOperation:
      n.inner.isVariardic
    of Operation:
      n.inner.isVariardic
    else:
      false
  

proc name*(n: Node): Node=
  case n.kind:
    of Empty: n
    of Ident: n
    of RegularOperation:
      n.inner.name
    of SpecialOperation:
      n[1].name
    of Operation:
      n.inner.name
    else: n.inner.name#.strVal

# proc `name=`*(n: sink Node, newN: Node)=
#   case n.kind:
#     of Empty, Ident:
#       n = newN
#     of RegularOperation:
#       n[0].name = newN
#     of SpecialOperation:
#       n[1].name = newN
#     of Operation:
#       n[0].name = newN
#     else: 
#       discard#.strVal

proc add*(self: var Node, other: Node): Node {.discardable.} =
  self.sons.add other

proc skipNodes*(n: Node, kinds: set[NodeKind]): Node =
  result = n
  while result.kind in kinds: result = result.inner
