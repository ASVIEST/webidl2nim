import std/sequtils
import std/sugar

import ast


func empty*(): Node=
  Node(kind: Empty)

func strLit*(s: string): Node=
  Node(kind: StrLit, strVal: s)

func boolLit*(b: bool): Node=
  Node(kind: BoolLit, boolVal: b)

func intLit*(i: int): Node=
  Node(kind: IntLit, intVal: i)

func floatLit*(f: float): Node=
  Node(kind: FloatLit, floatVal: f)

func specialKeyword*(kw: SpecialOperationKeyword): Node=
  Node(kind: Special, specOpKw: kw)



func ident*(name: string): Node =
  Node(kind: Ident, strVal: name)

func idents*(nodes: openArray[Node] = @[]): Node =
  #unsigned long long
  assert nodes.all(x => x.kind == Ident)

  Node(kind: Idents, sons: nodes.toSeq)

func generic*(base: Node, args: openArray[Node] = []): Node=
  assert base.kind == Ident
  assert args.all(x => x.kind == Type)

  Node(kind: Generic, sons: base & args.toSeq)

func generic*(base: Node, arg: Node): Node=
  generic(base, [arg])

func union*(types: openArray[Node]): Node=
  assert types.all(x => x.kind == Type)
  
  Node(kind: Union, sons: types.toSeq)



func typeStmt*(typeRest: Node; extendedAttributes = empty()): Node=
  assert typeRest.kind in {Ident, Idents, Generic, Union}
  assert extendedAttributes.kind in {Empty}

  Node(kind: Type, sons: @[typeRest, extendedAttributes])


func identDefs*(name, t: Node; default = empty()): Node=
  assert name.kind == Ident
  assert t.kind == Type
  assert default.kind in {Empty, IntLit, FloatLit, BoolLit, StrLit}

  Node(kind: IdentDefs, sons: @[name, t, default])


func constStmt*(identDefs: Node): Node=
  assert:
    identDefs.kind == IdentDefs and
    not identDefs[2].isEmpty

  Node(kind: ConstStmt, sons: @[identDefs])

func optionalArgument*(identDefs: Node): Node=
  assert identDefs.kind == IdentDefs
  Node(kind: OptionalArgument, sons: @[identDefs])

func ellipsis*(): Node=
  Node(kind: Ellipsis)

func callback*(name, signature: Node): Node=
  assert name.kind == Ident
  assert:
    signature.kind == Operation and 
    signature[0].kind == RegularOperation and
    signature[0][0].isEmpty
  
  Node(kind: Callback, sons: @[name, signature])

func simpleArgument*(identDefs: Node, ellipsis: Node = empty()): Node=
  assert identDefs.sons[2].isEmpty
  assert ellipsis.kind in {Ellipsis, Empty}

  Node(kind: SimpleArgument, sons: @[identDefs, ellipsis])

func argument*(arg: Node): Node=
  assert arg.kind in {OptionalArgument, SimpleArgument}
  Node(kind: Argument, sons: @[arg])

func argumentList*(args: openArray[Node]): Node=
  assert args.all(x => x.kind == Argument)
  Node(kind: ArgumentList, sons: args.toSeq)

func regularOperation*(name, t, args: Node): Node=
  assert name.kind in {Ident, Empty}
  assert t.kind == Type
  assert args.kind == ArgumentList

  Node(kind: RegularOperation, sons: @[name, t, args])

func specialOperation*(op, spec: Node): Node=
  assert spec.kind == Special
  assert op.kind == RegularOperation

  Node(kind: SpecialOperation, sons: @[op, spec])


func operation*(op: Node): Node=
  assert op.kind in {RegularOperation, SpecialOperation}

  Node(kind: Operation, sons: @[op])

func attribute*(name, t: Node): Node=
  assert name.kind == Ident
  assert t.kind == Type

  Node(kind: Attribute, sons: @[name, t])

func staticStmt*(member: Node): Node=
  assert:
    member.kind in {Attribute, RegularOperation} or
    (member.kind == Readonly and member.sons[0].kind == Attribute)
  
  Node(kind: Static, sons: @[member])

func required*(identDefs: Node): Node=
  assert:
    identDefs.kind == IdentDefs and
    identDefs.sons[2].kind == Empty

  Node(kind: Required, sons: @[identDefs])

func readonly*(member: Node): Node=
  assert member.kind in {Attribute, Maplike, Setlike}
    
  Node(kind: Readonly, sons: @[member])

func enumStmt*(name: Node, members: openArray[Node]): Node=
  assert name.kind == Ident
  assert members.all(x => x.kind == StrLit)

  Node(kind: Enum, sons: name & members.toSeq)

func typedef*(name, baseType: Node): Node=
  assert name.kind == Ident
  assert baseType.kind == Type

  Node(kind: Typedef, sons: @[name, baseType])

func includes*(dst, src: Node): Node=
  assert dst.kind == Ident
  assert src.kind == Ident

  Node(kind: Includes, sons: @[dst, src])

func namespaceMember*(member: Node): Node=
  assert:
    member.kind == ConstStmt or
    member.kind == Operation and member.sons[0].kind == RegularOperation or
    member.kind == Readonly and member.sons[0].kind == Attribute
  
  Node(kind: NamespaceMember, sons: @[member])

func namespace*(name: Node, members: openArray[Node]): Node=
  assert name.kind == Ident
  assert members.all(x => x.kind == NamespaceMember)

  Node(kind: Namespace, sons: name & members.toSeq)

func dictionary*(name, inheritance: Node, 
                 members: openArray[Node]): Node=
  assert name.kind == Ident
  assert inheritance.kind in {Ident, Empty}, "Expected Ident or Empty, but got " & $inheritance.kind
  assert members.all(x => x.kind == DictionaryMember)

  Node(kind: Dictionary, sons: @[name, inheritance] & members.toSeq)

func dictionaryMember*(member: Node): Node = 
  assert member.kind in {Required, IdentDefs}
  Node(kind: DictionaryMember, sons: @[member])

proc mixinStmt*(name: Node, members: openArray[Node]): Node =
  assert members.all(x => x.kind == MixinMember)
  Node(kind: Mixin, sons: @[name] & members.toSeq)

proc mixinMember*(member: Node): Node =
  assert:
    member.kind in {ConstStmt, Stringifier, Stringifier} or
    member.kind == Operation and member.inner.kind == RegularOperation or
    member.kind == Readonly and member.inner.kind == Attribute or
    member.kind == Attribute
  
  Node(kind: MixinMember, sons: @[member])


func stringifier*(attribute: Node): Node=
  assert:
    attribute.kind == Attribute or
    attribute.kind == Readonly and attribute.sons[0].kind == Attribute
  
  Node(kind: Stringifier, sons: @[attribute])

template keyValDecl(nodeKind: NodeKind, valueT: Node): Node=
  assert valueT.kind == Type

  Node(kind: nodeKind, sons: @[empty(), valueT])

template keyValDecl(nodeKind: NodeKind, keyT, valueT: Node): Node=
  assert valueT.kind == Type
  assert keyT.kind == Type

  Node(kind: nodeKind, sons: @[keyT, valueT])

func iterableStmt*(valueT: Node): Node= Iterable.keyValDecl(valueT)
func iterableStmt*(keyT, valueT: Node): Node= Iterable.keyValDecl(keyT, valueT)

func maplike*(keyT, valueT: Node): Node= Maplike.keyValDecl(keyT, valueT)
func setlike*(valueT: Node): Node= Setlike.keyValDecl(valueT)

func inherit*(attribute: Node): Node=
  Node(kind: Inherit, sons: @[attribute])

func interfaceStmt*(name: Node, inheritance: Node, 
                    members: openArray[Node]): Node=
  assert name.kind == Ident
  assert inheritance.kind in {Ident, Empty}
  assert members.all(x => x.kind == InterfaceMember)

  Node(kind: Interface, sons: @[name, inheritance] & members.toSeq)

func interfaceMember*(member: Node): Node=
  #! it not support async iterable
  assert:
    member.kind in {
      ConstStmt, Operation, 
      Stringifier, Static, 
      Readonly, 
      Iterable, Maplike, Setlike,
      Attribute,
      Inherit,
      Constructor, 
    }

  Node(kind: InterfaceMember, sons: @[member])

func partial*(definition: Node): Node=
  assert:
    definition.kind == Namespace or
    (definition.kind == Interface and
     definition.sons[2..^1].all(x => x.kind != Constructor)) or
    (definition.kind == Dictionary and definition.sons[1].isEmpty) or
    definition.kind == Mixin


  Node(kind: Partial, sons: @[definition])
