import macros, sequtils, strutils, sugar#, unode

proc processProc(i: NimNode): NimNode=
  let name = newStrLitNode i[0].repr
  let ident = ident"ident"

  let params = i.params[1..^1]
  let returnType = i.params[0]

  var paramsFlatten = newNimNode(nnkFormalParams)
  for i in params:
    let t = i[^2]
    for j in i[0..^3]:
      paramsFlatten.add newIdentDefs(j, t, i[^1])

  var pragmaStr = i.pragma[0][1].strVal
  for i, e in paramsFlatten:
    pragmaStr = pragmaStr.replace(e[0].strVal, "$" & $(i + 2))
  
  var generatedParams = newNimNode(nnkBracket)
  for i in paramsFlatten:
    # generatedParams.add 
    var (name, t, default) = (i[0], i[1], i[2])
    let ns = name.strVal
    var ident = quote: `ident`(`ns`)
    if name.strVal == "self":
      t = name
    
    if default.kind != nnkEmpty:
      generatedParams.add quote do:
        unode(unkIdentDefs).add(`ident`, `t`, `default`)
    else:
      generatedParams.add quote do:
        unode(unkIdentDefs).add(`ident`, `t`, empty())
  
  let pragma = quote:
    unode(unkExprColonExpr).add(`ident`("importjs"), strLit`pragmaStr`)
  
  var procName = quote:
    var name = `ident`(`name`)
    if exportCode:
      name = unode(unkPostfix).add(`ident`("*"), name)
    name

  result = quote:
    genRoutine(
      `procName`,
      `generatedParams`,
      empty(),      
    )

  if returnType.kind != nnkEmpty:
    let rtS = newStrLitNode(returnType.strVal)
    let rt = quote: `ident`(`rtS`)
    result.add rt
  else:
    result.add quote do: empty()
  
  result.add ident"unkProcDef"

  result.add quote do:
    unode(unkPragma).add(`pragma`)

macro signature*(name, body):untyped=
  let paramsStr = "self" & name[1..^1].mapIt(it.strVal)
  var iteratorBody = newStmtList()

  for i in body:
    iteratorBody.add newNimNode(nnkYieldStmt).add(i.processProc)
  
  var params = collect:
    for i in paramsStr:
      newIdentDefs(ident(i), ident"NimUNode", newEmptyNode())
  
  newProc(
    name[0].postfix "*", 
    @[
      ident"NimUNode", 
      newIdentDefs(ident("exportCode"), ident"bool", newEmptyNode())
    ] & params, 
    iteratorBody, 
    nnkIteratorDef
  )