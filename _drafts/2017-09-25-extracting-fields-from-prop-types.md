---
layout: post
title: "Extracting fields from prop types"
category: articles
tags: [coq]
comments: true
---

# Why destruct does not work

- rect, ind, rec
- :
- k# Simple example: bool is not unit

{% highlight coq %}
Definition id {T} := fun x : T => x.
 
Lemma exists_bijection (A B: Set):
A = B -> exists f, exists g, forall (x:A) (y:B),
 f x = y -> g y = x.
Proof.
 intro H; subst. 
 exists id.
 exists id.
 intros. subst. 
 reflexivity.
Qed.
{% endhighlight %}

The proof was a piece of cake. Now to battle!

You remember, that `unit` is a type, inhabited by only one element `tt`?


{% highlight coq %}
Inductive unit : Set := tt : unit.
{% endhighlight %}

Both `f true` and `f false` are evaluated to `tt`. There is not much choice here. However we know, that $$ \forall x, (g \cdot f) \ x = x$$ . By instantiating `x` with `true` and `false` we get $$g (f \ true) = true$$ and $$g (f \ false) = false$$. Since both $$ f \ true$$ and  $$f\ false$$ are equal, we deduce $$g\ tt =true$$ and $$g \ tt = false$$, contradiction.


Here is a little diagram to help:



Now the code:

{% highlight coq %}
Lemma unit_neq_bool: bool = unit -> False.
Proof.
intro Heq.
destruct (exists_bijection _ _ Heq) as [f [g Hfg]].
destruct (f true) eqn: Hft.
destruct (f false) eqn: Hff.
pose proof (Hfg _ _ Hft) as Hgtt.
pose proof ( Hfg _ _ Hff) as Hgtf.
 
rewrite Hgtf in Hgtt.
inversion Hgtt.
Qed.
{% endhighlight %}

So, the key idea is to enumerate possible bijections. As at least one of sets is finite, you will eventually enumerate all the candidates, and when the sets are of different size, you will get contradictions because you will run out of distinct functions trying to enumerate all bijections.


## Example: nat is not bool

Let us apply the same principle to prove `~ nat = bool`.

We are going to use a bit of semicolons because otherwise the proof will become very repetitive. Basically where you see an exclamation mark we got 8 goals which contexts include:

{% highlight coq %}
g x = 0
g y = 1
g z = 2
{% endhighlight %}

where `x`, `y`, `z` all range over $${ true, false}$$. There will always be at least two of three boolean values equal to each other (which should follow from Dirichlet principle), which will feed us with nice contradiction to do a rewrite and inversion.


{% highlight coq %}
Lemma nat_neq_bool: nat = bool -> False.
intro Heq.
destruct (exists_bijection _ _ Heq) as [f [g Hfg]].
 
destruct (f 0) eqn: Hf0;
destruct (f 1) eqn: Hf1;
destruct (f 2) eqn: Hf2;
pose proof (Hfg _ _ Hf0) as Hg0;
pose proof (Hfg _ _ Hf1) as Hg1;
pose proof (Hfg _ _ Hf2) as Hg2.  (* ! *)
rewrite Hg2 in Hg1; inversion Hg1.
rewrite Hg1 in Hg0; inversion Hg0.
rewrite Hg0 in Hg2; inversion Hg2.
rewrite Hg1 in Hg2; inversion Hg2.
rewrite Hg1 in Hg2; inversion Hg2.
rewrite Hg2 in Hg0; inversion Hg0.
rewrite Hg0 in Hg1; inversion Hg1.
rewrite Hg1 in Hg0; inversion Hg0.
Qed.
{% endhighlight %}

An interesting question (the answer to which I do not know) is whether we could automate it inside Coq somehow without writing plugins for it.

