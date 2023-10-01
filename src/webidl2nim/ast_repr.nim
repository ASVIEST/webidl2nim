import std/strformat
import std/[sequtils, strutils, sugar]

import ast

proc `$`*(node: Node): string=
  case node.kind:
    of Empty: ""
    of Ellipsis: "..."
    of Special: $node.specOpKw
    of Ident: node.strVal
    of StrLit: '"' & node.strVal & '"'
    of IntLit: $node.intVal
    of FloatLit: $node.floatVal
    of BoolLit: $node.boolVal

    of Type: fmt"{$node.sons[1]}{$node.sons[0]}"

    of IdentDefs:
      fmt"{$node.sons[1]} {$node.sons[0]}" & (
        if not node.sons[2].isEmpty:
          fmt" = {node.sons[2]}"
        else:
          ""
      )
    
    of Includes:
      fmt"{$node.sons[0]} includes {$node.sons[1]};"
    
    of Idents:
      node.sons.map(x => $x).join(" ")
    
    of Generic:
      $node[0] & "<" & node.sons[1..^1].map(x => $x).join(", ") & ">"
    
    of Iterable, Maplike, SetLike:
      if node[0].isEmpty:
        ($node.kind).toLower & "<" & $node.sons[1] & ">"
      else:
        ($node.kind).toLower & "<" & node.sons[0..1].map(x => $x).join(", ") & ">"
    
    of Inherit:
      fmt"inherit {$node[0]}"

    of Union:
      '(' & node.sons.map(x => $x).join(" or ") & ')'
    
    of Attribute:
      fmt"attribute {$node.sons[1]} {$node.sons[0]}"

    of Required:
      fmt"required {$node.sons[0]}"
    
    of Partial:
      fmt"partial {$node.sons[0]}"
  
    of Readonly:
      fmt"readonly {$node.sons[0]}"
    
    of Static:
      fmt"static {$node.sons[0]}"
    
    of Typedef:
      fmt"typedef {$node.sons[1]} {$node.sons[0]};"
    
    of OptionalArgument:
      fmt"optional {$node.sons[0]}"
    
    of Stringifier:
      fmt"stringifier {$node.sons[0]}"
    
    of ConstStmt:
      fmt"const {$node.sons[0]}"

    of SimpleArgument:
      $node[0][1] & (
        if not node[1].isEmpty:
          $node[1]
        else:
          ""
      ) & ' ' & 
      $node[0][0]
    
    of Argument:
      $node.sons[0]
    
    of Callback:
      fmt"callback {$node[0]} = {$node[1]}" 
    
    of RegularOperation:
      fmt"{$node.sons[1]}" & (
        if not node.sons[0].isEmpty:
          " " & $node.sons[0]
        else:
          " "
      ) & '(' & $node.sons[2] & ')'
    
    of SpecialOperation:
      fmt"{$node.sons[1]} {$node.sons[0]}"
    
    of Operation:
      $node.sons[0]
    
    of ArgumentList:
      $node.sons.map(x => $x).join(", ")

    of Enum:
      fmt"enum {$node.sons[0]} " & '{' & '\n' &
        node.sons[1..^1].map(x => indent($x, 2)).join(",\n") & 
      "\n};"
    
    of Namespace:
      fmt"namespace {$node.sons[0]} " & '{' & '\n' &
        node.sons[1..^1].map(x => indent($x, 2) & ";").join("\n") & 
      "\n};"
    
    of NamespaceMember, InterfaceMember, DictionaryMember:
      $node[0]
    
    of Dictionary:
      fmt"dictionary {$node.sons[0]}" & (
        if not node.sons[1].isEmpty:
          fmt": {$node.sons[1]} "
        else:
          " "
      ) &
      '{' & '\n' &
        node.sons[2..^1].map(x => indent($x, 2) & ";").join("\n") & 
      "\n};"

    of Interface:
      fmt"interface {$node.sons[0]}" & (
        if not node.sons[1].isEmpty:
          fmt": {$node.sons[1]} "
        else:
          " "
      ) &
      '{' & '\n' &
        node.sons[2..^1].map(x => indent($x, 2) & ";").join("\n") & 
      "\n};"

    else:
      "<invalid>"

when isMainModule:
  import ast_gen

  echo typedef(
    ident"GPUBufferUsageFlags",
    typeStmt(idents([ident"unsigned", ident"long"]), empty())
  )

  echo typedef(
    ident"GPUBindingResource",
    typeStmt(
      union [ident"GPUSampler".typeStmt, ident"GPUTextureView".typeStmt, ident"GPUBufferBinding".typeStmt],
    )
  )
  echo interfaceStmt(
    ident"A",
    ident"B",
    [constStmt(identDefs(ident"test", typeStmt(ident"string")))]
  )
