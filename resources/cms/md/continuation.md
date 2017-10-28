If there is no reused unit(e.g. function), all commands write one by one then, there is no need for continuation, all commands execute one by one until the end.

but a unit 'finished', it should not truly finished, only the unit finished, other commands after the unit invokation still need to be executed.

command a4, a5 locagically behind command b4 in the context of function A. But not addressly behind b4.
the sequence executer (get & inc program counter/instruction pointer/instruction address register/instruction counter, execute correpsonding command) after execute commands in function B by default not execute a4 a5, as there is no code addressly after a unit.

To make the programe continue, the last command(explictly or implicitly coded in source) in a unit should change program counter to where the unit is invoked.

The last command is continuation.




function A:
command a1
command a2
command a3
(function B)
command a4
command a5






function B:
command b1
command b2
command b3
command b4
