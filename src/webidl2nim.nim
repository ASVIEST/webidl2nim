import webidl2nim/[lexer, parser, translator, unode]

export translator
export unode

export parseCode
export tokenize

when isMainModule:
  # cli
  import std/[deques, sequtils, strutils, sugar, options, terminal]
  from os import walkPattern
  import pkg/[npeg, cligen]
  import packages/docutils/highlite
  import "$nim"/compiler/[ast, renderer]

  template writeColored(color: ForegroundColor, bright: bool = false, body: untyped) =
    if cliShowColor:
      stdout.setForegroundColor(color, bright)
    body

    if cliShowColor:
      stdout.resetAttributes()
    stdout.flushFile()
  
  proc writeCenter(s: string)=
    stdout.writeLine center(s, terminalWidth())

  proc writeSep() =
    stdout.writeLine "-".repeat(terminalWidth())
  
  template writeNimHighlight(code: string)=
    var toknizr: GeneralTokenizer
    initGeneralTokenizer(toknizr, code)
    while true:
      getNextToken(toknizr, langNim)
      case toknizr.kind
      of gtEof: break  # End Of File (or string)
      of gtWhitespace:
        stdout.resetAttributes()
        stdout.write substr(code, toknizr.start, toknizr.length + toknizr.start - 1)
      of gtOperator:
        var s = substr(code, toknizr.start, toknizr.length + toknizr.start - 1)
        
        writeColored(if s == "*": fgRed else: fgYellow, s == "*"):
          stdout.write substr(code, toknizr.start, toknizr.length + toknizr.start - 1)
      of gtDecNumber..gtFloatNumber, gtValue:
        writeColored(fgGreen, true):
          stdout.write substr(code, toknizr.start, toknizr.length + toknizr.start - 1)
      of gtKeyword:
        writeColored(fgBlue, true):
          stdout.write substr(code, toknizr.start, toknizr.length + toknizr.start - 1)
      of gtComment, gtLongComment:
        writeColored(fgBlack, true):
          stdout.write substr(code, toknizr.start, toknizr.length + toknizr.start - 1)
      else:
        stdout.resetAttributes()
        stdout.write substr(code, toknizr.start, toknizr.length + toknizr.start - 1)

  proc cli(
    features: set[Feature] = {
      ReadonlyAttributes, MethodCallSyntax, 
      ObjConstrRequired, NamespaceJsFieldBinding
    }, 
    outputFile = "stdout", 
    inputFile = "stdin",
    nep1 = true,
    exportCode = true,
    allowUndeclared = false,
    cliShowColor = true,
    cliOutFileListing = 10,
    optionalAttributePolicy = GenDeferredProcs
  ): string =
    var s = ""
    if inputFile == "stdin":
      writeColored(fgGreen, false):
        writeSep()
        writeCenter "Write webidl code: "
        writeSep()

      var enterCnt = 0
      while enterCnt < 2:
        let line = readLine(stdin)
        if line == "":
          inc enterCnt
        else:
          enterCnt = 0
        s.add line & "\n"

    else:
      for path in walkPattern(inputFile):
        let f = open(inputFile)
        s.add readAll(f)
        f.close()
    
    let t = tokenize s
    let c = parseCode(t).stack.toSeq
    var tr {.used.} = Translator(
      settings: TranslatorSettings(
        optionalAttributePolicy: optionalAttributePolicy,
        features: features,
        exportCode: exportCode,
        onIdent: (node: NimUNode, isDecl: bool) =>
          node
          .applyOn(nep1, (node: NimUNode) => nep1Rename(node, isDecl))
          .keywordToAccQuoted()
          .applyOn(exportCode, makePublic)
      ),
    )
    
    let outNode = tr.translate(c, allowUndeclared).assemble(tr.imports).to(PNode)
    let rendered = renderTree(outNode, {})
    if outputFile == "stdout":
      writeColored(fgYellow, false):
        writeSep()
        writeCenter "Output Nim code: "
        writeSep()
      writeNimHighlight(rendered)
      # stdout.write rendered
    else:
      renderModule(outNode, outputFile, {})
      writeColored(fgYellow, false):
        writeSep()
        writeCenter "Output Nim code successfully rendered into " & outputFile
      writeColored(fgYellow, true):
        if cliOutFileListing != 0:
          writeCenter "Small listing of this: "
      writeColored(fgYellow, false):
        writeSep()

      if cliOutFileListing != 0:
        var i = 0
        for line in splitLines(rendered):
          writeNimHighlight line
          stdout.write "\n"
          if i == cliOutFileListing:
            break
          inc i
        
        if i < countLines(rendered):
          stdout.writeLine "..."

  dispatch cli, help = {
    "features": "used webidl2nim features",
    "inputFile": "input file", 
    "outputFile": "output file",
    "nep1": "rename idents, following nep1", 
    "exportCode": "make generated code public",
    "allowUndeclared": "allow undeclared identifiers in webidl code"
  }
