---
layout: post
title: "How to prove certain type inequalities in Coq"
category: articles
tags: [coq]
comments: true
---


Sometimes you just want to prove that nasty   `~ T = U` for some types `T` and `U`. Well, while in general it is not decidable (nor provable), sometimes there is a relatively easy way to do it, when `T` and `U` are not both infinite. In other words, either `T` or `U`, or both of them should be finite.

 

# Simple example: bool is not unit
The idea is simple: if types are equal, there exists a bijection between their elements. Let us prove a simple lemma `exists_bijection`: 


$$ \forall A \ B: Set, A = B \rightarrow \exists f \exists g: \forall x:A \ \forall y : B, (f \ x = y -> g \ y = x ) $$

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

