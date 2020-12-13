---
layout: post
title: "When functions dissolve"
category: articles
tags: assembly, programming
comments: true
---

In this post we are going to explore how the notion of function loses its
significance as an abstracted module of program logic when we pass from a higher
level imperative language to the assembly code.

In imperative programming, functions isolate pieces of program logic.
For example, in C-like language, the following function would increase its argument and return its value.


```c
int f( int x ) {
   return x + 1;
}
```

Ideally, each function should have a single, well-defined goal and only contain
the code both necessary and sufficient to accomplish the said goal.

# Modularity 

There is a concept of *modularity* in system design. 
It means that to construct a system we start with smaller building blocks, which are built in isolation. 
Then we assemble the bigger system from these blocks.

Modularity can be more or less pronounced:

- *soft modularity* means that errors that appear in one module can propagate to other parts of the system. This propagation is out of control and lead to unpredictable consequences.
- *strong modularity* means that modules are more isolated and the error propagation is limited.

These two notions are pure and extreme, real systems often fall somewhere in
between.

An example of soft and strong modularity is seen in a computer which executes
several programs in parallel.
Each process is a component of a running system. 
If the processes share the same address space then the modularity is soft,
because an error in one process can lead to a data corruption of another.
If the processes have isolated virtual address spaces then the modularity is
hard, because an error in one process can not influence the memory in another
one. 
There are of course details to that, like when a process crashes while another
one is waiting for its response, but such situations can be dealt with in a
reasonable way e.g. through timeouts.

## Functions as modules

We are used to think about functions as the building blocks of program code.
Functions abstract away the complexity: a caller provides them with arguments,
and the function yields the computation result plus side-effects, like file
output.

In a high level language if we do not use global variables or resources
functions give us some reasonable modularity.
Any function can communicate failure to its caller through returning errors:
`NULL`, instances of optional types etc. (or exceptions).

## Higher level functions and lower level subroutines

The source code is eventually transformed into machine code. Without too much
oversimplification functions in higher level languages are translated into
functions in machine instructions. To make a distinction between them we will
call the lower level functions *subroutines*.

Subroutines should support two kinds of operations:

- we should be able to call subroutines from anywhere, and
- subroutines should be able to return to the place where they have been called.

On common architectures there are instructions or instruction patterns that are
used to call subroutines and return from them, like `call`/`ret` on Intel 64, or
`bl`/`pop pc` on ARM. 

The subroutines are different from functions in one important way:
subroutines operate in a common memory space with each other. 
Functions have local variables and arguments, but subroutines use CPU registers,
which are global and shared among them.

To make an analogy with higher-level language, imagine that:

- all your functions accept zero arguments;
- you are not allowed to create local variables;
- you have to repurpose the same global variables in different functions.

Because of this, modularity supported by subroutines is even "softer" than the function modularity.
In fact, I wonder if we can talk about any modularity at all (except that each
subroutine is written in one place in the source code which makes a subroutine a
structural module).
Maybe the better way to think about subroutines is to accept that they do not exist.
Once we get there, we may better understand how assembly programs are written,
how they function and what possibilities it offers to us.

# Exemplary assembly program

The rest of this post uses the assembly code as a device to tell about two
interesting concepts: tail-call elimination and coroutines. 
We are going to optimize the subroutine `print_newline` in several stages up to
the point that it is going to lose its shape as an isolated subroutine.


```nasm

; Gets symbol code in rdi and writes it in stdout
print_char: 
   ...
   ; the code of print_char is not important to us
   ...
   ret

; Prints newline character
print_newline: 
    mov rdi, 0xA          ; code 
    call print_char
    ret
```


## Tail call optimization

Tail call happens when the subroutine ends with a call to another subroutine.
In the higher level language this happens in cases such as:

```c
void g();

void f() {
    g(); 
    return; 
}
```

This is also a tail call, because we just return what another function returns:

```c
int g(int x);

int f() {
    return g(42); 
}
```

This is not a tail call: after calling function we have to multiply its result to another number. 
Note that in this example the function calls itself rather than some other
function, but this is not important.

```c
int fact( int n ) {
  if (n < 2) return 1;
  return n * fact(n-1);
} 
```

On the assembly level, a tail call corresponds to the following pattern of instructions:

```nasm
f: 
  ...
  call other
  ret
```

This pair of instructions is located in a function which we are going to call `f`.
The control reaches this function from some other function, say,  `f_caller`.

Let us follow the state of stack through the execution of `f_caller`, `f` and `other`.
For that we expand this code to include `f_caller` and `other` functions.

```nasm
f_caller:

...
   call f
   <next instruction> ; mark 4
...

f:            ; mark 1
  ...
  call other 
  ret         ; mark 3
...
other:        ; mark 2
   ...
   ret
```

- When we start executing `f` the stack holds the return address inside `f_caller` (we reach  mark 1).
- When we call `other` the stack holds the return addresses for `f_caller`, then `f` on top (we reach mark 2).
- The subroutine `other` returns too, in this moment we have only return address for `f_caller` on top of the stack  (we reach mark 3).
- The subroutine `f` returns, popping return address of `f_caller` from the stack  (we reach mark 4).

The last two steps were essentially popping two return addresses from stack consecutively.
It suggests that the first one (the return address to `f`) is useless and we do not need to store it.
We are indeed right.

The `call other` instruction is equivalent to the pair of pseudoinstructions:

```nasm
push rip    ; rip is the program counter register
jmp other 
```

If we do not need to push return address to `f`, then we can just substitute this instruction for `jmp` and get rid of one `ret`:


```nasm
f_caller:

...
   call f
   <next instruction>
...

f:
  ...
  jmp other 
...
other:
   ...
   ret     ; will return directly to f_caller
```

The subroutine `other` becomes essentially the continuation of the subroutine `f`; we pass from `f` to `other` via a simple branching instruction.


Coming back to our example, we can rewrite it like that:

```nasm
print_char: 
   ...
   ret

print_newline: 
    mov rdi, 0xA          ; code 
    jmp print_char
```

### Tail recursion 

What if `other` and `f` are the same function?
Then the `jmp` is performed to the start of `f` making the whole thing into a loop:


```nasm
f_caller:

...
   call f
   <next instruction>
...

f:
  ...
  jmp f
```

This is what we mean when we say that the compiler optimizes tail recursion into a loop.


## Coroutines


Our current example is the implementation of `print_newline` in two instructions:

```nasm
print_char: 
   ...
   ret

print_newline: 
    mov rdi, 0xA          ; code 
    jmp print_char
```

When the example is written like this it is easy to guess the next step. 
If the execution is sequential why bother with a jump here? 
The control routinely falls from one instruction to the next one anyway.


```nasm
print_newline: 
    mov rdi, 0xA          ; code 
print_char: 
   ...
   ret
```

The subroutines have merged together here and it does not look like there are
two separate functions anymore.
We got a subroutine with **two entry points** labeled `print_newline` and `print_char`. 
We call such a thing a *coroutine*.

Coroutines are a generalization of subroutines, they may have a state and
multiple entry points. You may often encounter them or their variations in
modern languages e.g. generators in Python or lazy `IEnumerable`s in C#. Our
coroutine does not have a state, however. It is funny to note that coroutines
appeared first as a assembly programming pattern and is not a fancy new thing
like cubical type theory provers.

If your language supports continuations, you can implement coroutines easily.

# Conclusion

I do like to draw connections between seemingly distant notions and areas. 
I also enjoy learning new points of view on things I already know.
My students inspired me to write this post because for a lot of them, coming
from higher level languages, the functions were entities with well shaped
bodies, having a visible start and end.
Well, in assembly they do not have to.

It also supports a way of thinking about compiled code as a "soup" of sorts,
where everything is blended together, reordered, precomputed, optimized until
only the actual actions and their order suggest how the original program looked
like. I think this is especially useful when reasoning about parallel programs
and lock-free algorithms.



