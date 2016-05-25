[![Build Status](https://travis-ci.org/zjhmale/Clea.svg?branch=master)](https://travis-ci.org/zjhmale/Clea)

well... just explore the good part (extremely flexible, concise syntax, compile-time metaprogramming) and the bad part (dynamic type, impure, sometimes too much parentheses) of Lisp, nothing fancy here.

## Example

```clojure
(defmacro when
  [test & body]
  `(if ~test (do ~@body)))

(ƒ non-sense
  [y]
  (λ [x] (when y (prn "y ⇔ ⊤") x)))

((non-sense true) 3)
```
