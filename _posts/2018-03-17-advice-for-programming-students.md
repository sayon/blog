---
layout: post
title: "Advice for programming students"
category: articles
tags: programming, studying
comments: true
---

There are many things I wish I knew when I started my journey in IT. This posts
collects some of advice I'd gladly given to my past self.

There are a lot of paths in IT and you certainly do not need to be familiar with
formal logic if you want to just know one practical thing and do it. In other
words, chose your path and chose wisely:

* You can become proficient in one domain relatively fast (say one, two years)
  and know how to do certain things -- enough to make a living.
 
* You can become a well rounded specialist that can adapt to anything and
  dynamically switch career paths, e.g., do machine learning, then formal
  verification, than do low-level programming for trading or work in game dev.
  That demands time and dedication (I’d estimate at least 6–8 years to gain the
  foundation).
 
I strongly advocate the second path because it’s more versatile, interesting,
brings more in the long run (IT is ever-changing so you want to pick up new
technologies fast).

Now, important points to consider for the ones who took the second path.

* **Learn math because math is useful**. I can not stress that enough. You might
  think that you don’t need linear algebra, but if you do non-trivial machine
  learning, you need it. You need statistics and probability. You need logic,
  combinatorics, all sorts of discrete mathematics, graph theory, computability,
  formal grammars, lambda calculus, topology, type theories, a bit of number
  theory, groups, rings, fields, categories, formal semantics.

  New technologies are constantly emerging in IT. Many of them are based on the
  mathematics. If you know the underlying mathematics well, you get some perks:
  Picking new trendy things is orders of magnitude simpler. You immediately
  understand where you can apply them and where you should not. You usually
  understand why are they the way they are and can tweak them to better suit the
  context. For example, from my experience, few people understand, that you
  should not always use least squares to evaluate how well your linear
  regression fits the data. This is only sane when the errors are distributed
  normally with the appropriate mean value. Were you in other situation, you
  will not even know that there is a part of typical linear regression that
  should be tweaked.
 
* **Learn math to learn mathematical thinking.** Writing proofs makes you rigorous.
  You want to always think about all possible traces your program can take, in
  order to not introduce bugs and security issues. The clarity of thinking
  gained from constructing proofs is precious. It also helps you in writing
  short, concise code.
 
* **Pick your first language carefully**. It should be well designed, which means:
    * Consistency
    * Small core
    * No unnecessary complexity
    * Makes it harder to shoot yourself in the foot.

  This should also be a high level language, because programming is problem
  solving, not a mastery of a specific tool. Try one of these languages:
    * Scheme (there is a good classical introductory course called SICP)
    * Smalltalk
    * Eiffel

   Don’t be fooled by their seeming unpopularity, that is not correlated with
   quality.

   *Do not start with Python, pretty please!* It is badly designed,
   inconsistent, and does not teach you rigorous thinking. 
 
* **If you get used to crap** languages and crap tools, and crap software, and
  crap solutions, **you will inevitably replicate*** them in your own work. Be
  critical, question everything, critic everything, search for inconsistencies
  and flaws.

  Let’s say, you are learning a new language, Go. Google "Go sucks" and read why
  people criticise it. Some of them will be pathetic, but some will actually
  have a point.

* **Learn algorithms and data structures**. Pick up a good book and solve
  contests online. I recommend “Algogithms” of Dasgupta for a start, then the
  classic work of Cormen. This will open a whole new world for you.

* **WRITE FROM SCRATCH**. The types of skills needed for that are so different
  from copying another guy’s solution from Stack Overflow and changing it to
  your needs! Programming is about making conscientious choices. When you start
  writing programs from scratch, it will be hard, but it is absolutely necessary
  to learn to build things from zero.
 
 
* **Forget about code camps**. You need to learn fundamental concepts, then the
   rest is a breeze.

* **Program everyday, do side projects all the time.** There is a very easy (and
  mostly accurate) way for me as a teacher to understand that my student will
  probably succeed as a programmer. One question: *What are you programming for
  pleasure?*

  You should ideally touch everything around: write your own compiler, maybe a
  toy OS, http server, database engine, games, ray casting, build some neural
  networks, fiddle with proof assistants and dependent types, write a simple
  mobile app, write for embedded ... I could go on.

* **Expose yourself to different tools and languages**.  If someone tells you
  all languages are alike, this is in fact not true. A model of computation is a
  set of basic operations and ways of gluing them together in order to build
  complex algorithms from them. Some languages have very similar models of
  computations, some are very different.

   Programming is so much bigger than your commonly known
   C/Python/Java/C++/C#/Go/Javascript, which are all built on the same
   principles: imperative, structural, with occasional bits of OOP and syntactic
   sugar to mimic other programming styles. How about:
    * Functional programming (Haskell, Ocaml)?
    * Functional languages with dependent types, which allow not only to program, but to write proofs of correctness (Coq, Agda, LEAN)
    * Stack based, concatenative languages (Forth)
    * Logic programming (Prolog, Refal)?
    * Finite state machines (regular expressions, Promela)?
    * Event driven programming? Actors?
    * Heavily extensible languages allowing to implement arbitrary syntax, as Lisp, Forth, Camlp4/5 or Rebol allow? This is a whole universe to explore!

  Every new model of computations is hard to learn, because it is a new way of
  thinking for you. But the investment is worth your time, because once you get
  familiar with it,

    * Every language based on it is easy
    * Every language whose model of computations has similarities is easier.
    * As every model of computations is very fitting for specific type of problems to solve, you now have a new powerful tool, whose usage in specific contexts is orders of magnitude more productive.

* **Ask better people to do code reviews** for you. Read well-written code.
* **Stick with passionate, smart people, and try learning from them**. You will
  be surprised, how much can you learn during a lunch time with your mates, who
  are eager to share the details of their work or research. This kind of
  cross-pollination is one of the main reasons corporations like Google give you
  free quality food.

I hope that this might actually help someone to get a bigger picture. Good luck!
