import object_signatures_dsl, unode

signature setlike[T]:
  proc incl(self; value: T) {.js: "self.add(value)".}
  proc excl(self; value: T) {.js: "self.delete(value)".}
  proc `in`(value: T, self): bool {.js: "self.has(value)".}
  proc clear(self) {.js: "self.clear()".}
  proc len(self) {.js: "self.size()".}
  # forEach

signature readonlySetlike[T]:
  # forEach
  proc `in`(value: T, self): bool {.js: "self.has(value)".}
  proc len(self) {.js: "self.size()".}