import object_signatures_dsl, unode

signature setlike[T]:
  proc incl(self; value: T) {.js: "self.add(value)".}
  proc excl(self; value: T) {.js: "self.delete(value)".}
  proc `in`(value: T, self): bool {.js: "self.has(value)".}
  proc clear(self) {.js: "self.clear()".}
  proc len(self): int {.js: "self.size()".}
  # forEach

signature readonlySetlike[T]:
  # forEach
  proc `in`(value: T, self): bool {.js: "self.has(value)".}
  proc len(self) {.js: "self.size()".}

signature maplike[T]:
  # forEach entries, keys, values
  func entries(self): seq[tuple[key, value: cstring]] {.js: "Array.from(self)".}
  proc `[]=`(self; key, value: T) {.js: "self.set(key, value)".}
  proc `[]`(self; key: T): T {.js: "self.get(key)".}
  proc `in`(key: T, self): bool {.js: "self.has(key)".}
  proc del(self; key: T) {.js: "self.delete(key)".}
  proc clear(self) {.js: "self.clear()".}
  proc len(self): int {.js: "self.size()".}

signature readonlyMaplike[T]:
  # forEach entries, keys, values
  func entries(self): seq[tuple[key, value: cstring]] {.js: "Array.from(self)".}
  proc `[]`(self; key: T): T {.js: "self.get(key)".}
  proc `in`(key: T, self): bool {.js: "self.has(key)".}
  proc len(self): int {.js: "self.size()".}
