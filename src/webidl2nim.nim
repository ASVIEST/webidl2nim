import webidl2nim/[lexer, parser, translator, unode]

export translator
export unode

export parseCode
export tokenize

when isMainModule:
  # cli
  import std/[deques, sequtils, strutils, sugar, options, terminal]
  import pkg/[npeg, cligen]

  import "$nim"/compiler/renderer

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

  proc cli(
    features: set[Feature] = {}, 
    outputFile = "stdout", 
    inputFile = "stdin",
    nep1 = true,
    exportCode = true,
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
      let f = open(inputFile)
      s = readAll(f)
      f.close()
    
    let t = tokenize s
    let c = parseCode(t).stack.toSeq
    var tr {.used.} = Translator(
      settings: TranslatorSettings(
        optionalAttributePolicy: optionalAttributePolicy,
        features: features,
        onTypeDef: (node: NimUNode, typeDefKind: NimUNodeKind) =>
          node
          .applyOn(nep1, nep1Rename)
          .applyOn(exportCode, makePublic)
      ),
    )
    
    let outNode = tr.translate(c).assemble(tr.imports).toPNode
    let rendered = renderTree(outNode, {})
    if outputFile == "stdout":
      writeColored(fgYellow, false):
        writeSep()
        writeCenter "Output Nim code: "
        writeSep()
      stdout.write rendered
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
          stdout.writeLine line
          if i == cliOutFileListing:
            break
          inc i
        
        if i < countLines(rendered):
          stdout.writeLine "..."

  dispatch cli
