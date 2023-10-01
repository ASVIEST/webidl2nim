import tokens
import std/[strutils, sequtils, sugar, tables]
import pkg/regex

const 
  punctuators = {
    "(": tLPar,
    ")": tRPar,
    ",": tComma,
    "...": tEllipsis,
    ":": tColon,
    ";": tSemiColon,
    "<": tLess,
    "=": tAssign,
    ">": tMore,
    "?": tQuestion,
    "*": tMul,
    "[": tLBracket,
    "]": tRBracket,
    "{": tLCurly,
    "}": tRCurly,
  }.toSeq

  keywords = collect:
    for i in tAsync..tRecord:
      ($i, i)
  
  alpha = {'a'..'z', 'A'..'Z', '0'..'9'}

  reTokens = (
    ident: re2"[_-]?[A-Za-z][0-9A-Z_a-z-]*",
    str: re2("\\\"[^\\\"]*\\\""),
    comment: re2"\/\/.*|\/\*[\s\S]*?\*\/",

    intNorm: re2"-?[1-9][0-9]*",
    intOct:  re2"-?0[Xx][0-9A-Fa-f]+",
    intHex:  re2"-?0[0-7]*"
  )

import options
type
  Lexer* = ref object
    buffPos: int

template cmpStrings(
  self: var Lexer; 
  s: string, 
  tokS: string
): bool =
  tokS.len <= s.len - self.buffPos and 
  s[self.buffPos..self.buffPos + tokS.high] == tokS

proc tryParsePunctuator(self: var Lexer; s: string): Option[Token]=
  for (i, token) in static(punctuators):
    if self.cmpStrings(s, i):
      self.buffPos += i.len
      return some(Token(kind: token))

proc tryParseKeyword(self: var Lexer; s: string): Option[Token]=
  # let isBounded = 
  for (keyword, token) in static(keywords):
    if self.cmpStrings(s, keyword):
      if s.len > self.buffPos + keyword.len and s[self.buffPos + keyword.len] in alpha:
        return

      self.buffPos += keyword.len
      return some(Token(kind: token))

proc tryParseRegex(self: var Lexer; s: string, r: Regex2): Option[string] =
  let s1 = s[self.buffPos..^1]
  if s1.startsWith(r):
    var m = RegexMatch2.default()
    discard find(s1, r, m)
    m.boundaries = m.boundaries.a + self.buffPos .. m.boundaries.b + self.buffPos
    self.buffPos += m.boundaries.len
    return some(s[m.boundaries])

proc tryParseIntLit(self: var Lexer; s: string): Option[int] =
  if (let i = self.tryParseRegex(s, reTokens.intNorm); i).isSome:
    some(parseInt(i.get))
  elif (let i = self.tryParseRegex(s, reTokens.intOct); i).isSome:
    some(parseHexInt(i.get))
  elif (let i = self.tryParseRegex(s, reTokens.intHex); i).isSome:
    some(parseOctInt(i.get))
  else:
    none(int)

proc eatWhitespace(self: var Lexer; s: string): bool =
  for i in s[self.buffPos..^1]:
    if i in {'\t', '\n', '\r', ' '}:
      inc self.buffPos
      result = true
    else:
      break

# unsignedlonglong
iterator lex(self: var Lexer; s: string): Token =
  while self.buffPos < s.len:
    if (let token = self.tryParsePunctuator(s); token).isSome:
      yield token.get()
    elif (let token = self.tryParseKeyword(s); token).isSome:
      yield token.get()
    elif self.eatWhitespace(s): discard
    elif (let s = self.tryParseRegex(s, reTokens.ident); s).isSome:
      yield Token(kind: tIdentifier, strVal: s.get())
    elif (let s = self.tryParseRegex(s, reTokens.str); s).isSome:
      yield Token(kind: tString, strVal: s.get()[1..^2])
    elif (let i = self.tryParseIntLit(s); i).isSome:
      yield Token(kind: tInteger, intVal: i.get())
    elif (let comment = self.tryParseRegex(s, reTokens.comment); comment).isSome:
      discard
    else:
      raise newException(CatchableError, "Lexing error")

    # if 

proc tokenize*(str: string): seq[Token] {.inline.} =
  var lexer = Lexer(buffPos: 0)
  for i in lexer.lex(str):
    result.add i

when isMainModule:
  var lexer = Lexer(buffPos: 0)
  
  for i in lexer.lex"""typedef Int8Array ArrayBufferView;""":
    echo i, ":", i.kind
