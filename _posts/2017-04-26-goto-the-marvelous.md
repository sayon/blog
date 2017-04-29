---
layout: post
title: "goto, the marvelous"
category: articles
tags: [c, c++, goto]
comments: true
---

IT folks are prone to prejudices.  Once a beginner programmer starts exploring the world of coding, he quickly learns catchy memes from the bigger, more experienced part of the community: "Code should be simple above all", "There is an OOP pattern for that", "C is fast" and lots of other simple rules.  One can easily live by them without putting much thought in their meaning. Looks totally like things from human society, when we adopt elements of our parents' lifestyle and world view, making them parts of ourselves, usually not questioning them. However, there is one fundamental empirical rule, that there are no silver bullets -- rules, that are always applicable. Each of these memes requires a certain context to be fully understood. My opinion is that we should always try to reach these deep foundations in order to understand when the rule can be thrown away for good.  This post is about an absurd meme I will call "goto is evil". We want to explore what exactly makes `goto` bad. 

## Why goto is discouraged in the structured programming

Structured programming is a programming style that (as each style) implies a set of rules to live by. In structured programming we make extensive use of subroutines, loops and code blocks. It usually goes in par with imperativeness, when each programming statement is executed sequentially, and mutable addressable memory -- roughly following the von Neumann model of computations. It is safe to say that one of its most devoted supporters was [Edsger W. Dijkstra](https://en.wikipedia.org/wiki/Edsger_W._Dijkstra). Object-oriented programming can be viewed as a slightly tweaked version of it. A large slice of modern programming is structured imperative programming or its variations; moreover, it is what kids are taught in school so most of us are deeply infected with an imperative thinking and are often emulating other paradigms on top of it in our heads.

In general, Dijkstra considered `goto` harmful because it makes harder to follow the code logic. This is usually true, but needs explanation. What makes `goto` harmful is that it is usually paired with assignments.

Assignments change the state. When reasoning about a typical  program, we usually trace it and mark how the values of the variables are changing. Throwing `goto`’s everywhere makes it notably harder to follow the state changes, because you can jump anywhere, from anywhere. Given a label, we can not instantly understand, from where are there jumps to it. 
This is why in assembly language listings people often draw the arrows to illustrate the jumps: it helps tracing the origins of each jump.

However, throw away the state changes and you will have no problems using multiple goto’s in an isolated piece of code (e.g. inside a particular function), because the interesting state will be determined solely by your current position in code!



## goto in Finite State Machines

If we want to implement a [Finite State Machine (FSM)](https://en.wikipedia.org/wiki/Finite-state_machine), then `goto`’s are the way to go! A such abstract machine consists of:

- a set of states (C labels). One state is marked as an initial state.
- input (sequence of global events, f.e., character input, received network packets, any user actions)
- output (sequence of global actions, responses of the system: sending packets, control signal to the connected hardware, if we are building an embedded system etc)
- a set of rules to change the current state based on the input element.

We start in the initial state and perform jumps between states based on the current input value. As you see, this machine has no memory, and its state is solely characterized by the position in the code we are currently at.

Crafting a FSM is equivalent to crafting an algorithm to solve a problem. They are not expressive enough to solve all problems Turing-machines can digest. Nevertheless they are not only potent, but very convenient for some tasks such as template matching in strings, network protocols and robot controlling tasks.

Here is a toy example, taken from [this book](http://www.apress.com/us/book/9781484224021). It checks whether the input string containing only characters 0 and 1 contains an even number of ones. It is common to draw nice diagrams for FSM, such as this one, showing states as circles and transitions based on input symbols.


![fsm_example](/images/posts/2017-04-26-goto-the-marvelous/fsm_parity_example.png) 

Here is the implementation in C. I have omitted error checks for brevity.

{% highlight c %}
#include <stddef.h>
#include <stdio.h>
 
/* str should only contain 0 and 1 */
int even_ones( char const* str ) {
    size_t i = 0;
    char input;
_even:
    input = str[i++];
    if (input == '1') goto _odd;
    if (input == '0') goto _even; 
    /* end of string -- null terminator */
    return 1; 
_odd:
    input = str[i++];
    if (input == '1') goto _even;
    if (input == '0') goto _odd;
    /* end of string -- null terminator */
    return 0; 
}
 
void test( const char* str ) {
    printf("%s\n", even_ones( str ) ? "yes" : "no" );
}
 
int main(void) {
    test("0101011");
    test("");
    test("010");
    test("110");
 
    return 0;
}
{% endhighlight %}

### Finite State Machines and model checking
An additional benefit is that there exist a quite powerful verification technique called model checking which allows you to reason about the program properties if is possible to encode the said program as a finite automaton. Furthermore, there are tools to generate C code automatically for a drawn diagram. It is easy to visualize the bugs this way, because in many cases they are of type "For a state S we have no rule for a possible global event E".

TODO an example

## Deinitializing resources

C++ has a nice feature C lacks. It can automatically call object destructors whenever the object's lifetime is over. For example, in the following code the destructor for an object `myC` will be automatically called after we reach the closing bracket.  But it gets better: every time you are writing a return statement, everything that exists in the surrent stack frame gets automatically destroyed in the correct order (reversed to the initialization order).  Consider this function, which returns the error code and uses three objects: `myA`, `myB` and `myC`.


{% highlight c %}
#include <iostream>

int f() {
    A myA;
    B myB;
    C myC;

    if (! myA.init() ) return 0;
    if (! myB.init() ) return 0; // myA's destructor is called automatically
    if (! myC.init() ) return 0; // myA and myB's destructors are called automatically
    
    //...

    // Destructors for myA, myB, myC will be called here anyway
    return 1;
}
{% endhighlight %}

In C we often want to do the same thing, but we do not have that luxury of automatically called destructors. It is, however, very important do to because some structures have dynamically allocated fields or are associated with other resources such as file descriptors.

{% highlight c %}
int f() {
    struct sa a;
    struct sb b;
    struct sc c;

    if ( ! sa_init( &a ) ) return 0;
    if ( ! sb_init( &b ) ) { sa_deinit( &a ); return 0; }
    if ( ! sc_init( &c ) ) { sb_deinit( &b ); sa_deinit( &a ); return 0; }

    
    return 1;
}
{% endhighlight %}

Imagine you had 5 structures to work with, this straightforward approach is going to turn your code into nightmare! However, with the help of `goto`s we are going to exploit a nice little trick. It bases on the fact that all such branches can be ordered by inclusion: each branch looks exactly  like some other branch preceded by an additional `deinit`:

{% highlight c %}
-
                                   return 0;
                  sa_deinit( &a ); return 0;
sb_deinit( &b ); sa_deinit( &a ); return 0;
{% endhighlight %}

If we throw labels in between we could jump to any statement in this sequence. This way we are going to refactor the example above to look like this:

{% highlight c %}
int f() {
    struct sa a;
    struct sb b;
    struct sc c;

    if ( ! sa_init( &a ) ) goto fail;
    if ( ! sb_init( &b ) ) goto fail_b;
    if ( ! sc_init( &c ) ) goto fail_c;

    return 1;

fail_c:
    sb_deinit( &b ); 
fail_b:
    sa_deinit( &a );
fail:
    return 0;
}
{% endhighlight %}

Isn't it way nicer that what we have seen before? Additionally, no assignments are performed hence no fuss about `goto` evilness at all.


## Computed goto

Computed `goto`s are a non-standard feature supported by many popular C and C++ compilers. Basically, it allows to store a label into a variable and perform jumps to it. It differs from calling function by pointer because no return is ever performed.  The simplest case is shown below:

{% highlight c %}
#include <stdio.h>

int main() {
    void* jumpto = &&label;

    goto *jumpto;
    puts("Did not jump");
    return 0;

label: 
    puts("Did jump");
    return 1;
}
{% endhighlight %}

We are taking raw label address using an unusual double ampersand syntax and then perform a `goto` on it. Notice the additional asterisk before `goto` operand. When launched, this program will output `Did jump`.  
Where can we use it? There is nothing we can gain from it comparing to existing language constructions, but we surely can gain performance sometimes. A case that comes to my mind is a bytecode interpreter (but I have written hell of a ton of them, so I should be quite biased). The instruction fetching takes typically no less than 30% of the execution time, and computed `goto`s allow one to speed up exactly this part of program execution.  

Without computed `goto`s:


{% highlight c %}
#include <stdio.h>
#include <inttypes.h>

enum bc { BC_PUSH, BC_PRINT, BC_ADD, BC_HALT };

uint8_t program[] = { BC_PUSH, 1, BC_PUSH, 41, BC_ADD, BC_PRINT, BC_HALT };

void interpreter( uint8_t* instr ) {
    uint8_t stack[256];
    uint8_t* sp = stack;

    for (;;)
        switch ( *instr )  {
        case BC_PUSH: 
            instr++;
            sp++;
            *sp = *instr;
            instr++;
            break;

        case BC_PRINT:
            printf( "%" PRId8 "\n", sp[0] );
            instr++;
            break;

        case BC_ADD: 
            sp--;
            sp[0] += sp[1];
            instr++;
            break;

        case BC_HALT:
            return; 
        }
}

int main() {
    interpreter( program );
    return 0;
}
{% endhighlight %}

With computed gotos:

{% highlight c %}
#include <stdio.h>
#include <inttypes.h>

enum bc { BC_PUSH, BC_PRINT, BC_ADD, BC_HALT };

uint8_t program[] = { BC_PUSH, 1, BC_PUSH, 41, BC_ADD, BC_PRINT, BC_HALT };

void interpreter( uint8_t* instr ) {
    uint8_t stack[256];
    uint8_t* sp = stack;

    void* labels[] = { &&label_PUSH, &&label_PRINT, &&label_ADD, &&label_HALT };

    goto *labels[*instr];

label_PUSH:
    instr++;
    sp++;
    *sp = *instr;
    instr++;
    goto *labels[*instr];

label_PRINT:
    printf( "%" PRId8 "\n", sp[0] );
    instr++;
    goto *labels[*instr];

label_ADD: 
    sp--;
    sp[0] += sp[1];
    instr++;
    goto *labels[*instr];

label_HALT:
    return; 
}

int main() {
    interpreter( program );
    return 0;
}
{% endhighlight %}

We replaced `switch` with an array storing the address of an instruction handler. Each time we fetch an instruction, we are using its bytecode as an offset in this array. After taking an address from there, we jump to it.  The larger the instruction set and the `switch` gets, the more noticeable gets the difference in a real world program.



