{.used.}
import pkg/npeg

grammar definitions:
  Definitions <-
    *(extended_attributes.ExtendedAttributeList * Definition)
  Definition <-
    callback.Callback |
    interfaces.Interface |
    partial.Partial |
    namespace.Namespace |

    dictionary.Dictionary |
    enums.Enum |
    typedef.Typedef |
    includes.IncludesStatement# |
    

grammar extended_attributes:
  ExtendedAttributeList <- ?(
    [tLBracket] * ExtendedAttribute * *([tComma] * ExtendedAttribute) * [tRBracket]
  )
  
  ExtendedAttribute <-
    ([tLPar] * ?ExtendedAttributeInner * [tRPar] * ?ExtendedAttribute) |
    ([tLBracket] * ?ExtendedAttributeInner * [tRBracket] * ?ExtendedAttribute) |
    ([tLCurly] * ?ExtendedAttributeInner * [tRCurly] * ?ExtendedAttribute) |
    Other * ?ExtendedAttribute

  ExtendedAttributeInner <-
    ([tLPar] * ?ExtendedAttributeInner * [tRPar] * ?ExtendedAttributeInner) |
    ([tLBracket] * ?ExtendedAttributeInner * [tRBracket] * ?ExtendedAttributeInner) |
    ([tLCurly] * ?ExtendedAttributeInner * [tRBracket] * ?ExtendedAttributeInner) |
    OtherOrComma * ?ExtendedAttributeInner
  
  Other <-
    [tInteger] |
    [tDecimal] |
    [tIdentifier] |
    [tString] |
    [tOther] |
    [tColon] |
    [tSemiColon] |
    [tDot] |
    [tEllipsis] | #TODO: add * 
    [tLess] |
    [tAssign] |
    [tMore] |
    [tQuestion]


  OtherOrComma <- (Other | [tComma])

grammar types:
  Type <- (
    SingleType |
    (UnionType * Null)
  ):
      capture typeStmt(p.pop())
  
  TypeWithExtendedAttributes <-
    extended_attributes.ExtendedAttributeList * Type
  
  SingleType <- (
    DistinguishableType |
    >[tAny] |
    PromiseType
  ):
    if capture.len > 1:
      capture ident($ $1)
  
  UnionType <- (
    [tLPar] * UnionMemberType * 
    [tOr] * 
    UnionMemberType  * *([tOr] * UnionMemberType) * [tRPar]
  ): capture union(popSameKindNodes(Type))

  UnionMemberType <- (
    (extended_attributes.ExtendedAttributeList * DistinguishableType) |
    UnionType * Null
  ): capture typeStmt(p.pop())

  DistinguishableType <- (
    PrimitiveType |
    StringType |
    BufferRelatedType |
    DistinguishableTypeGenerics |
    RecordType |
    >[tIdentifier] |
    >[tObject] |
    >[tSymbol] |
    >[tUndefined]
  ) * Null:
    if capture.len > 1:
      capture ident($ $1)

  DistinguishableTypeGenerics <- (
    (>[tSequence]        * [tLess] * TypeWithExtendedAttributes * [tMore]) |
    (>[tFrozenArray]     * [tLess] * TypeWithExtendedAttributes * [tMore]) |
    (>[tObservableArray] * [tLess] * TypeWithExtendedAttributes * [tMore])
  ):
    capture generic(
      ident($ $1),
      p.pop()
    )
  
  PrimitiveType <- (
    UnsignedIntegerType |
    UnrestrictedFloatType |
    >[tBoolean] |
    >[tByte] |
    >[tOctet] |
    >[tBigint]
  ):
    if capture.len > 1:
      capture ident($ $1)

  UnrestrictedFloatType <- (
    (>[tUnrestricted] * FloatType) |
    FloatType
  ): genIdents(capture)
  
  FloatType <- >([tFloat] | [tDouble])

  UnsignedIntegerType <- (
    (>[tUnsigned] * IntegerType) |
    IntegerType
  ): genIdents(capture)

  IntegerType <-
    >[tShort] |
    (>[tLong] * ?>[tLong])

  StringType <- (
    >[tByteString] |
    >[tDOMString] |
    >[tUSVString]
  ): capture ident($ $1)

  PromiseType <- (
    >[tPromise] * [tLess] * Type * [tMore]
  ):
    capture generic(
      ident($ $1),
      p.pop()
    )

  RecordType <- (
    >[tRecord] * [tLess] * StringType * [tComma] * TypeWithExtendedAttributes * [tMore]
  ):
    let
      t2 = p.pop()
      t1 = typeStmt p.pop() #StringType is ident

    capture generic(
      ident($ $1),
      [t1, t2]
    )
  
  BufferRelatedType <- >(
    [tArrayBuffer] |
    [tDataView] |
    [tInt8Array] |
    [tInt16Array] |
    [tInt32Array] |
    [tUint8Array] |
    [tUint16Array] |
    [tUint32Array] |
    [tUint8ClampedArray] |
    [tBigInt64Array] |
    [tBigUint64Array] |
    [tFloat32Array] |
    [tFloat64Array]
  ): capture ident($ $1)

  Null <- ?[tQuestion]


grammar constants:
  Const <- (
    [tConst] * ConstType * >[tIdentifier] * [tAssign] * 
    ConstValue * ([tSemiColon] | E"; after const decl not found")
  ):
    let val = p.pop()
    capture constStmt identDefs(
      ident ($1).strVal,
      p.pop(),
      val
    )
  
  ConstValue <- (
    BooleanLiteral |
    FloatLiteral |
    >[tInteger]
  ):
    if capture.len > 1:
      capture intLit(($1).intVal)

  BooleanLiteral <- >([tTrue] | [tFalse]):
    capture boolLit(
      case ($1).kind:
        of tTrue:
          true
        of tFalse:
          false
        else:
          raise newException(CatchableError, "Non bool tokens in boolLit")
    )
  
  FloatLiteral <- >(
    [tDecimal] |
    [tMinusInfinity] |
    [tInfinity] |
    [tNaN]
  ):
    capture floatLit(
      case ($1).kind:
        of tDecimal:
          ($1).floatVal
        of tMinusInfinity:
          NegInf
        of tInfinity:
          Inf
        of tNaN:
          NaN
        else:
          raise newException(CatchableError, "Non float tokens in floatLit")
    )
  
  ConstType <- (
    types.PrimitiveType |
    ConstTypeIdent
  ): capture typeStmt(p.pop())

  
  ConstTypeIdent <- >[tIdentifier]:
    capture ident($ $1)

grammar arguments:
  ArgumentList <- ?(Argument * *([tComma] * Argument)):
    capture argumentList(popSameKindNodes(Argument))
  
  Argument <-  extended_attributes.ExtendedAttributeList * ArgumentRest:
    capture argument(p.pop())

  ArgumentRest <-
    SimpleArgument |
    OptionalArgument
  
  OptionalArgument <- (
    [tOptional] * types.TypeWithExtendedAttributes * 
    ArgumentName * default.Default
  ):
    let default =
      if (%1).kind == Type:
        empty()
      else:
        p.pop()
  
    capture optionalArgument(
      identDefs(
        ident($ $1),
        p.pop(),
        default
      ),
    )
  
  SimpleArgument <- (types.Type * ?>[tEllipsis] * ArgumentName):
    let
      isVariardic = capture.len > 2
      nameToken =
        if isVariardic:
          $2
        else:
          $1
    
    #TODO: transform variardic(with ...) to nim varargs

    capture simpleArgument(
      identDefs(
        ident($nameToken),
        p.pop()
      ),

      if isVariardic:
        ellipsis()
      else:
        empty()
    )

  ArgumentName <-
    ArgumentNameKeyword |
    >[tIdentifier]


  ArgumentNameKeyword <- >(
    [tAsync] |
    [tAttribute] |
    [tCallback] |
    [tConst] |
    [tConstructor] |
    [tDeleter] |
    [tDictionary] |
    [tEnum] |
    [tGetter] |
    [tIncludes] |
    [tInherit] |
    [tInterface] |
    [tIterable] |
    [tMaplike] |
    [tMixin] |
    [tNamespace] |
    [tPartial] |
    [tReadonly] |
    [tRequired] |
    [tSetlike] |
    [tSetter] |
    [tStatic] |
    [tStringifier] |
    [tTypedef] |
    [tUnrestricted]
  )

  
  

grammar attributes:
  ReadWriteAttribute <- AttributeRest
  InheritAttribute <- [tInherit] * AttributeRest:
    capture inherit(p.pop())
  
  OptionalReadOnlyAttribute <- ReadOnlyAttribute | AttributeRest
  ReadOnlyAttribute <- [tReadonly] * AttributeRest:
    capture readonly(p.pop())

  AttributeRest <- [tAttribute] *  types.TypeWithExtendedAttributes * AttributeName * [tSemiColon]:
    capture attribute(ident($ $1), p.pop())

  AttributeName <- AttributeNameKeyword | >[tIdentifier]
  AttributeNameKeyword <- >(
    [tAsync] |
    [tRequired]
  )

grammar namespace:
  Namespace <- (
    [tNamespace] * >[tIdentifier] * [tLCurly] * 
    NamespaceMembers * 
    [tRCurly] * [tSemiColon]
  ):
    capture namespace(
      ident ($1).strVal,
      popSameKindNodes(NamespaceMember)
    )

  NamespaceMembers <-
    *(extended_attributes.ExtendedAttributeList * NamespaceMember)
  
  NamespaceMember <- (
    operations.RegularOperation |
    ReadonlyAttribute |
    constants.Const
  ):
    var node = p.pop()
    if node.kind == RegularOperation:
      node = operation(node)
    
    capture namespaceMember(node)

  ReadonlyAttribute <- [tReadonly] * attributes.AttributeRest:
    capture readonly(p.pop())


grammar maplike:
  ReadWriteMaplike <- MaplikeRest
  MaplikeRest <- (
    [tMaplike] *
    [tLess] * types.TypeWithExtendedAttributes * 
    [tComma] * 
    types.TypeWithExtendedAttributes * [tMore] * [tSemiColon]
  ):
    let
      valT = p.pop()
      keyT = p.pop()

    capture maplike(keyT, valT)

grammar setlike:
  ReadWriteSetlike <- SetlikeRest
  SetlikeRest <-
    [tSetlike] * [tLess] * types.TypeWithExtendedAttributes * [tMore] * [tSemiColon]:
      capture setlike(p.pop())

grammar readonly:
  ReadOnlyMember <- [tReadonly] * ReadOnlyMemberRest:
    capture readonly(p.pop())

  ReadOnlyMemberRest <-
    attributes.AttributeRest |
    maplike.MaplikeRest |
    setlike.SetlikeRest

grammar stringifier:
  Stringifier <- [tStringifier] * (StringifierWithAttribute | [tSemiColon])

  StringifierWithAttribute <- attributes.OptionalReadOnlyAttribute:
    capture stringifier(p.pop())

grammar static_member:
  StaticMember <- [tStatic] * StaticMemberRest
  StaticMemberRest <- (
    attributes.OptionalReadOnlyAttribute |
    operations.RegularOperation
  ): capture staticStmt(p.pop())

grammar iterable:
  #! iterable implemented as generic
  Iterable <- IterableRest * [tSemiColon]:
    capture:
      if p.stack.len > 1 and (%2).kind == Type:
        let
          valT = p.pop()
          keyT = p.pop()

        iterableStmt(keyT, valT)
      else:
        iterableStmt(p.pop())

  IterableRest <- [tIterable] * [tLess] * 
  types.TypeWithExtendedAttributes * *([tComma] * types.TypeWithExtendedAttributes) *
  [tMore]

grammar async_iterable:
  AsyncIterable <- [tAsync] * iterable.Iterable * ?([tLPar] * arguments.ArgumentList * [tRPar])

grammar constructor:
  Constructor <- [tConstructor] * [tLPar] * arguments.ArgumentList * [tRPar]

grammar operations:
    Operation <- (
      RegularOperation |
      SpecialOperation
    ): capture operation(p.pop())

    RegularOperation <- types.Type * OperationRest:
      var
        args = p.pop()
        name = p.pop()
        t    = p.pop()  

      capture regularOperation(
        name,
        t, args
      )

    SpecialOperation <- Special * RegularOperation:
      capture specialOperation(
        p.pop(),
        specialKeyword(
          case ($1).kind:
            of tGetter:
              Getter
            of tSetter:
              Setter
            of tDeleter:
              Deleter
            else:
              raise newException(CatchableError, "Invalid keyword in special")
        )
      )

    Special <- >([tGetter] | [tSetter] | [tDeleter])

    OperationRest <-
      OperationName * [tLPar] * arguments.ArgumentList * [tRPar] * [tSemiColon]
    
    OperationName <- ?(
      OperationNameKeyword |
      >[tIdentifier]
    ):
      capture:
        if capture.len > 1:
          ident($ $1)
        else:
          empty()
    
    OperationNameKeyword <-
      >[tIncludes]



grammar inheritance:
  Inheritance <- ?InheritanceRest:
    capture:
      if capture.len > 1:
        ident($ $1)
      else:
        empty()

  InheritanceRest <- [tColon] * >[tIdentifier]

grammar default:
  Default <- ?([tAssign] * DefaultValue)
  DefaultValue <-
    constants.ConstValue |
    DefaultString |
    ([tLBracket] * [tRBracket]) |
    ([tLCurly] * [tRCurly]) |
    [tNull] |
    [tUndefined]
  
  DefaultString <- >[tString]:
    capture strLit($ $1)

grammar dictionary:
  PartialDictionary <- ([tDictionary] * >[tIdentifier] * [tLCurly] * DictionaryMembers * [tRCurly] * [tSemiColon]):
    capture dictionary(
      ident($ $1),
      empty(),
      popSameKindNodes(DictionaryMember)
    )

  Dictionary <- ([tDictionary] * >[tIdentifier]  * inheritance.Inheritance * [tLCurly] * DictionaryMembers * [tRCurly] * [tSemiColon]):
    let members = popSameKindNodes(DictionaryMember)
    var inheritance = p.pop()
    if inheritance.kind == Type: inheritance = inheritance.inner

    capture dictionary(
      ident($ $1),
      inheritance,
      members
    )

  DictionaryMembers <- *(extended_attributes.ExtendedAttributeList * MemberRest)
  MemberRest <-
    RequiredMember |
    TypeMember
  
  TypeMember <- types.Type * >[tIdentifier] * default.Default * [tSemiColon]:
    let (default, t) =
      if (%1).kind == Type:
        # no default val
        (empty(), p.pop())
      else:
        (p.pop(), p.pop())

    capture dictionaryMember identDefs(ident($ $1), t, default)
  RequiredMember <-
    ([tRequired] * types.TypeWithExtendedAttributes * >[tIdentifier] * [tSemiColon])

grammar enums:
  Enum <- (
    [tEnum] * >[tIdentifier] * [tLCurly] * EnumValueList * [tRCurly] * [tSemiColon]
  ):
    let members =
      collect:
        for i in 2 ..< capture.len:
          strLit capture[i].s.strVal

    capture enumStmt(
      ident ($1).strVal,
      members
    )

  # Name <- >[tIdentifier]
  EnumValueList <- >[tString] * *([tComma] * >[tString]) * ?[tComma]#{"str1", "str2"}

grammar typedef:
  Typedef <- (
    [tTypedef] * types.TypeWithExtendedAttributes * >[tIdentifier] * [tSemiColon]
  ): capture typedef(ident ($1).strVal, p.pop())

grammar includes:
  IncludesStatement <- >[tIdentifier] * [tIncludes] * >[tIdentifier] * [tSemiColon]:
    capture includes(ident($ $1), ident($ $2))

grammar mixins:
  MixinRest <- [tMixin] * >[tIdentifier] * [tLCurly] * MixinMembers * [tRCurly] * [tSemiColon]:
    capture mixinStmt(ident($ $1), popSameKindNodes(MixinMember))

  MixinMembers <- *(extended_attributes.ExtendedAttributeList * MixinMember)
  MixinMember <- (
    constants.Const |
    operations.RegularOperation |
    stringifier.Stringifier |
    (?[tReadonly] * attributes.AttributeRest)
  ):
    let member = 
      if (%1).kind == RegularOperation:
        operation p.pop()
      else:
        p.pop()

    capture mixinMember(member)
  

grammar partial:
  Partial <- [tPartial] * PartialDefinition
  PartialDefinition <- (
    ([tInterface] * PartialInterfaceOrPartialMixin) |
    dictionary.PartialDictionary |
    namespace.Namespace
  ): capture partial(p.pop())

  PartialInterfaceOrPartialMixin <-
    PartialInterfaceRest |
    mixins.MixinRest

  
  PartialInterfaceRest <-
    (>[tIdentifier] * [tLCurly] * PartialInterfaceMembers * [tRCurly] * [tSemiColon]):
      capture interfaceStmt(
        ident($ $1),
        empty(),
        popSameKindNodes(InterfaceMember)
      )
      
  PartialInterfaceMembers <-
    *(extended_attributes.ExtendedAttributeList * PartialInterfaceMember)
  
  PartialInterfaceMember <- (
    constants.Const |
    operations.Operation |
    stringifier.Stringifier |
    static_member.StaticMember |
    iterable.Iterable |
    async_iterable.AsyncIterable |
    readonly.ReadOnlyMember |

    attributes.ReadWriteAttribute |
    maplike.ReadWriteMaplike |
    setlike.ReadWriteSetlike |

    attributes.InheritAttribute
  ): capture interfaceMember(p.pop())

grammar interfaces:
  Interface <- [tInterface] * InterfaceOrMixin
  InterfaceOrMixin <-
    InterfaceRest |
    mixins.MixinRest

  InterfaceRest <- (
    >[tIdentifier] * inheritance.Inheritance * [tLCurly] * InterfaceMembers * [tRCurly] * [tSemiColon]
  ):
    let
      members = popSameKindNodes(InterfaceMember)
      inheritance = p.pop()

    capture interfaceStmt(
      ident ($1).strVal,
      inheritance,
      members
    )

  InterfaceMembers <-
    *(extended_attributes.ExtendedAttributeList * InterfaceMember)

  InterfaceMember <- (
    partial.PartialInterfaceMember |
    constructor.Constructor
  ):
    if (%1).kind != InterfaceMember:
      # for constructor
      capture interfaceMember(p.pop())

grammar callback:
  #TODO: add callback inteface support
  Callback <- [tCallback] * CallbackRest#CallbackRestOrInterface

  CallbackRestOrInterface <- CallbackRest
  CallbackRest <- >[tIdentifier] * [tAssign] * types.Type * [tLPar] * arguments.ArgumentList * [tRPar] * [tSemiColon]:#operations.RegularOperation:
    let
      args = p.pop()
      t    = p.pop()
    
    capture callback(
      ident ($1).strVal,
      operation regularOperation(empty(), t, args)
      # p.pop()
    )
