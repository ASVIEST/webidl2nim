# Package

version       = "0.0.1"
author        = "ASVIEST"
description   = "webidl to Nim bindings generator"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["webidl2nim"]


# Dependencies

requires "nim >= 1.9.1"
requires "regex"
requires "npeg"
requires "cligen"
