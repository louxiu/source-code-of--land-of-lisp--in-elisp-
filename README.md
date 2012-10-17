I want to rewrite the common lisp code of the book “land of lisp” in elisp.
This below is the source code in elisp and some difference between common lisp and elisp.

1.Difference between common lisp and elisp

1.to represent character ‘a’, common lisp use #\a, while elisp use ?a.
2.function ‘format’ in elisp is different from ‘format’ in common lisp.
3.there is no fresh-line function in elisp. but it is very simply to implement yours.
4.the defstruct or defclass are all vector, so you can not use override (overload?) in elisp as common lisp.
5.there is no ‘code-char’ function. Actually the char is integer in elisp.

2.source code written in elisp (partly)

2012年 10月 1日 月曜日 14:12:45 CST
chapter 2: guess.el
chapter 5 & 6: wizards_game.el
chapter 7: graph-util.el
chapter 8: wumpus.el

2012年 10月 2日 火曜日 10:04:46 CST
chapter 9: orc-battle.el

2012年 10月 2日 火曜日 11:09:08 CST
chapter 10: evolution.el

3.Reference

source written in common lisp
convert symbol to string