# webidl2nim
Tool to translate webidl code to Nim (js target).
## Quickstart
```nim
import webidl2nim
import std/[deques, sequtils, sugar]
import pkg/npeg

let t = tokenize"""
interface Hello-Webidl {
};
"""
let c = parseCode(t).stack.toSeq

let translator {.used.} = Translator(
  settings: TranslatorSettings(
  optionalAttributePolicy: GenDeferredProcs,#UseDefaultVal,#GenDeferredProcs,
    features: {
      MethodCallSyntax, 
      NamespaceJsFieldBinding, 
      ObjConstrRequired,
      ReadonlyAttributes
    },
    onTypeDef: (node: NimUNode, typeDefKind: NimUNodeKind) =>
      node
      .nep1Rename
      .makePublic
  ),
)

import "$nim"/compiler/renderer
echo translator.translate(c).assemble().toPNode
```
Output:
```nim
type
  HelloWebidl = ref object of JsRoot
```
