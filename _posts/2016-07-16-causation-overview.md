---
layout: post
title: "Causation in modern philosophy of science"
category: articles
tags: [philosophy of science, causation]
comments: true
---

Our minds use the notions of cause and consequences all the time. It is, without
a doubt, one of the fundamentals of human reasoning. However, when observed
closely and formally, it becomes apparent that our concept is based rather on
intuition and lacks strictness and connection to the real word. This post is
intended as a quick introduction to causation from the philosophical point of
view.


## General thoughts


1. Where to establish the causal relation? We are going to use propositions, which
 represent _quantitative properties of various systems in a given moment of time_,
 that is, _measurable properties_. Events represents the changes in these
 properties. These events will be selected as causes and consequences.  This way
 we are building an expressive language to construct statements of a scientific
 value. Contrary, people often think about events that can either happen or not.
 In our system such events are easily modeled as Boolean properties (taking
     value of either 0 or 1).
2. There exist two seemingly equally justified points of view on the place of
causal relations in the world.
  * There are common causation laws that rule the world; then they are
    instantiated in different situations with different events.  
  * Causes and consequences are an indispensable part of the world, all 
    generalizations are secondary.


## Hume's arguments


The arguments we are going to study can partly be traced to XVIII century! Meet
David Hume, a philosopher.

Hume’s account on causes and consequences is highly empiric. What an observer
(even ideal one) can observe about two events A and B is:

* _A_ occured before _B_.
* _A_ and _B_ are close in space and time. Or they are connected by a chain of
 events, where each link is a relation between two events which satisfies
 these properties.
* When we observe something resembling _A_ again, something resembling _B_ appears
   as well.

It is suspicious that these three observations sum up everything an observer
can notice --- even an ideal observer. There is no way this information might
be of a foundation for a strict causal relation as we usually imagine it.

Adding causality changes absolutely nothing here, because it does not give us
anything observable.

In fact the pattern above can describe many events not necessarily connected in
any way. Hume’s opinion is a great starting point, but now let us also address
some problems about it.

Three properties of _(A,B)_  pinpointed above occur not only in cases where we
want to establish the relation, but also:

1. When _A_ and _B_ have a common cause
2. Just by coincidence
3. By preemption. It means that there is an event _C_ that occurred before _A_
and would have caused _B_ anyway.

These three major problems are addressed differently depending on how one sees
causation in general, which features of the cause-consequence pair are really
key. Let’s now speak about those ways.



## Sufficiency
To this day people tend to think about causation as a deterministic beast. Our
knowledge about micro world, however, contradicts it (as many things about
micro world contradict common sense). Contrary to some traditional views on
causation based on _necessity of the cause to bring about its effect_, their
modern counterparts fit mostly in two categories:

1. Those who base on regular occurrences of _A_ followed by _B_.
2. Those who reconstruct the events of real world in the other, purely logical world.

Let’s take a closer look at the second point. In order to construct our logical
structures we can use certain sets of rules (they can represent f.e. the laws
of nature) of form:

$$ \forall x, F \ x \Rightarrow G \ x $$

Here for an event _a_ the expression  _F a_ will be substituted by an event
instance; _G a_  is the consequence. As for the arrow, we can substitute it for
either a material conditional (think _simple implication_) or something
stronger like a _subjunctive conditional_ ("If Oswald hadn’t killed Kennedy,
someone else would have").



## Necessity
You know, sufficiency is not _sufficient_ for causation. Even if we stick to
determinism, the overdetermination (multiple causes) alone is a valid reason to
question it.  Let us say, _A_ and _B_ can both equally cause _C_ and they occurred
simultaneously. What caused _C_?

Basing on sufficiency of cause we can deduce:

* If _A_ was the cause, then _C_ would not have occurred without _A_. So _A_ is not the cause.
* If _B_ was the cause, then _C_ would not have occurred without _B_. So _B_ is not the cause.

Some people tend to think that it’s rather the cause’s necessity for the
consequence that forms a casual relation between them. This way _A_ caused _B_
if and only if:

* _A_ and _B_ occurred.
* We can assert:  "If _A_ had not occurred, than _B_ would not have occurred" (we will refer to it as _sine qua non_, because, well, its  what it is).

I guess it should look like: $$A \land B \land ( \neg A \rightarrow \neg B)$$.

It is but a foundation of a longer talk I intend to give in the next post based
on arguments of Lewis and maybe Mackie (if I do have time for his book).


## Probabilistic approach
 
Reasoning about _sine qua non_ is not easy as long as you abandon determinism.
Non-determinism, however, goes in pair with probabilities. The general idea of
this approach is that causes increase probabilities of consequences in a large
variety of contexts: 

$$ P ( B / AZ ) > P (B / \neg AZ )$$

Here _Z_ should take into account:

* Common causes of _A_ and _B_.
* Preemptive causes of _B_.


This way preemption problem and common cause problem are addressed, and _A_ and _B_ 
become probabilistically independent.

The hard thing is to choose _Z_ and a good definition of probability.

For example, take relative frequencies for probability. This way we should
exclude from _Z_ all causal consequences of _B_ for which _B_ is necessary. If we do
not do it, we just get a wrong inequality _1 > 1_ (check it!). However, look at
it again: to calculate _P_ we need exactly causal data for which we are building
a theory! We have faced a vicious circle that is not easy to break.

## Conclusion
As we see, there are loads of interesting subtleties when it comes to a closer
study of causation. Numerous attempts were made to exile this relation
completely from scientific thinking, but its roots are so strong it proved
almost impossible to do. Next time I want to talk about Lewis’s very influential
theory on causation, dated 1973, and then about his new causation theory of
early 2000’s.
