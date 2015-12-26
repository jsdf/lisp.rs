# lisp.rs

[lis.py](http://norvig.com/lispy.html) ported to rust


```scheme
jsdf:lisp_rs jfriend🔥  cargo run

lisp.rs> (+ 2 2)
=> 4
lisp.rs> (begin (define r 10) (* pi (* r r))) 
=> 314.1592653589793
lisp.rs> (if (> 10 20) (+ 1 1) (+ 3 3))
=> 6
```