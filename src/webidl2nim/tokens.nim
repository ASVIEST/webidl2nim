import std/sugar

type
  TokenKind* = enum
    tInteger
    tDecimal
    tIdentifier
    tString
    tWhitespace
    tComment
    tOther

    tComma = ","
    tColon = ":"
    tSemiColon = ";"
    tEllipsis = "..."
    tDot = "."
    tMul = "*"
    tQuestion = "?"
    

    tAssign = "="
    tMore = ">"
    tLess = "<"

    tLPar = "("
    tRPar = ")"

    tLBracket = "["
    tRBracket = "]"

    tLCurly = "{"
    tRCurly = "}"
    
    #ArgumentNameKeyword
    tAsync = "async"
    tAttribute = "attribute"
    tCallback = "callback"
    tConst = "const"
    tConstructor = "constructor"
    tDeleter = "deleter"
    tDictionary = "dictionary"
    tEnum = "enum"
    tGetter = "getter"
    tIncludes = "includes"
    tInherit = "inherit"
    tInterface = "interface"
    tIterable = "iterable"
    tMaplike = "maplike"
    tMixin = "mixin"
    tNamespace = "namespace"
    tPartial = "partial"
    tReadonly = "readonly"
    tRequired = "required"
    tSetlike = "setlike"
    tSetter = "setter"
    tStatic = "static"
    tStringifier = "stringifier"
    tTypedef = "typedef"
    tUnrestricted = "unrestricted"
    tUnsigned = "unsigned"
    tOptional = "optional"

    tAny = "any"
    tOr = "or"

    tTrue = "true"
    tFalse = "false"

    tNaN = "NaN"
    tInfinity = "Infinity"
    tMinusInfinity = "-Infinity"

    #Types
    tSequence = "sequence"
    tObject  = "object"
    tSymbol = "symbol"
    tFrozenArray = "FrozenArray"
    tObservableArray = "ObservableArray"
    tUndefined = "undefined"
    tNull = "null"
    tBoolean = "boolean"
    tByte = "byte"
    tOctet = "octet"
    tBigint = "bigint"
    tFloat = "float"
    tDouble = "double"
    tShort = "short"
    tLong = "long"
    #Buffer related type
    tArrayBuffer = "ArrayBuffer"
    tDataView = "DataView"
    tInt8Array = "Int8Array"
    tInt16Array = "Int16Array"
    tInt32Array = "Int32Array"
    tUint8Array = "Uint8Array"
    tUint16Array = "Uint16Array"
    tUint32Array = "Uint32Array"
    tUint8ClampedArray = "Uint8ClampedArray"
    tBigInt64Array = "BigInt64Array"
    tBigUint64Array = "BigUint64Array"
    tFloat32Array = "Float32Array"
    tFloat64Array = "Float64Array"

    tByteString = "ByteString"
    tDOMString = "DOMString"
    tUSVString = "USVString"

    tPromise = "Promise"
    tRecord = "record"

  Token* = object
    case kind*: TokenKind:
      of tInteger:
        intVal*: int
      of tDecimal:
        floatVal*: BiggestFloat
      of tIdentifier, tString:
        strVal*: string
      else:
        discard

func `==`*(n: Token, t: TokenKind): bool = n.kind == t
proc `$`*(t: Token): string=
  case t.kind:
    of tInteger: $t.intVal
    of tDecimal: $t.floatVal
    of tIdentifier: t.strVal
    of tString: '"' & t.strVal & '"'
    else: $t.kind

const keywordNames* = collect:
  for i in tAsync..tRecord:
    $i
