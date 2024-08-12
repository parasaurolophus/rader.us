# Computability Theory

> _Or, How Euclid's Parallel Postulate Gave Rise to the Information Age_

$$
\begin{align}
\text{Let } \aleph_0 & = \omega \ \omega \\
\text{where } \omega &= \lambda x . x \ x
\end{align}
$$

Once upon a time, mathematicians in the Western tradition regarded their
profession as being a science, no different from chemistry or physics. That
view was based on an empirical understanding of mathematical truths. In this
model, _2 + 2 = 4_ is true because if I am holding two pebbles and pick up two
more, then I can confirm by counting the pebbles in my hand that I end up
holding four.

Euclid's _Elements_ was firmly based in this empirical tradition. While he
provided arguments which, in modern terms, were based on a formal approach to
proving theorems based on a set of axioms, his proofs relied on "constructions"
using experimentally verifiable procedures involving straight-edge and compass
to justify not just his premises but also his conclusions.

Euclid famously defined a number of axioms (in the form of "postulates") based
on such constructions. Loosely translated into modern terms, his five axioms
are:

1. A point can be extended indefinitely to form a line.
2. A line can be extended indefinitely to form a plane.
3. A point can be rotated at a constant distance around another point to form a
   circle.
4. All right angles are equal to each other.
5. If two lines intersect a third line such that the interior angles on the
   same side are each less than a right angle, the first two lines will
   eventually intersect when they are both extended indefinitely.

The fifth axiom has historically been referred to as the "parallel postulate,"
and was the source of obsession by countless mathematicians over the millenia
starting in Euclid's time and continuing up to the relatively recent past. Note
how different it is from the other four axioms. Axioms 1 - 4 are short
statements of easily visualized constructions. The parallel postulate, on the
other hand, is quite verbose by comparison and takes a few readings to fully
grasp the construction... at which point it becomes as obviously true as the
others when one is drawing figures on a flat surface:

![](/parallel_postulate.svg)

It seems so complex by comparison to the others and has an air of somehow
depending on them (by reference to extending lines and comparing right angles)
that for a very long time mathematicians suspected that it must not actually be
an axiom at all. Volumes were written on various attempts to discover the
simpler axioms from which the parallel postulate could be derived as a theorem.

Along the way, various alternative axioms were identified as being equivalent
to Euclid's. Playfair's Theorem is the most famous one, and is presented in
many modern texts on geometry as the preferred "fifth axiom":

![](/playfair_theorem.svg)

All such attempts failed to produce truly simpler axioms because, as it turns
out, the parallel postulate really is an axiom and not a theorem hiding some
unknown number of real axioms under its skirts.

This was proven in the 19th century by the likes of Bolyai, Lobachevsky and
Riemann. Their approach was clever, and as hard to understand initially as the
parallel postulate itself. The various attempts to simplify Euclid's version of
the axiom had given mathematicians practice in devising alternative forms of
the parallel postulate and proving whether or not they were consistent with the
rest of Euclid's system of geometry. Bolyai, Lobachevsky, Riemann and others
reasoned that if the parallel postulate was actually a theorem, then if you
tried to work out a complete system of geometry using some different axiom that
contradicted Euclid's fifth axiom but kept the other four intact, then you
would inevitably be led to some self-contradiction between the four "real"
axioms and your "bogus" one. What Bolyai and company set out to prove (since by
then it was obvious the parallel postulate really was an axiom and all that was
left was _proving_ it in a fashion acceptable to mathematicians) was that you
can create any number of internally consistent versions of geometry using
axioms that differ only slightly from Euclid's. Each of them will produce a
different set of theorems from Euclid's and from each others', but none of them
will produce theorems that contradict _themselves_ when considered
independently of one another. I.e. this is an indirect proof of a particular
kind: by proving that there are multiple possible geometries based on different
variations of Euclid's parallel postulate, the latter is thus proven actually
to be an axiom and not a theorem in disguise.

But what, exactly, is meant by "multiple possible geometries" when the whole
point of Euclid's constructions is to root his particular axioms in empirical
observations involving a straight edge and compass? In order to accept this
whole approach, mathematicians were forced to reject empiricism. The new
"Non-Euclidean" geometries were accepted as logically valid even though they
produced results that could not be proven to be true experimentally (at the
time, see below about later applications in fields like Einstein's General
Relativity) using mechanical tools like Euclid's straight edge and compass. In
order to accept the very satisfactory result to the millenia old concerns about
the parallel postulate mathematicians had to once and for all acknowledge that
all that really mattered to them was _validity_ (i.e. the internal logical
consistency of a set of axioms), not empirical _truth_. Science is the province
of empiricism, so mathematics ceased to be regarded as a science.

This distinction was at the heart of an explosion in mathematical practice and
techniques that began in the late 19th century and which, ultimately, provided
the theoretical basis for scientific and technological advances like General
Relativity, Quantum Physics and Computer Science. Paradoxically, the focus on
_metamathematics_ (the study of mathematics as purely formal systems divorced
from empirical reality) is what made possible the very palpable advancements we
now take for granted.

## Infinite Quantities and the Continuum Hypothesis

One immediate effect of the distinction between "applied" and "pure"
mathematics was that it gave permission for mathematicians to consider problems
that had traditionally been considered "out of bounds" because they did not
correspond to empirically observable conditions. When the truth of _2 + 2 = 4_
was believed to rely on counting things in the real world, then all one can say
of infinite quantities is that they do not exist in the real world and so
mathematics can have nothing to say about them. After all, you can never hold
an infinite number of pebbles in your hand, count an infinite number of pebbles
and so on.

But once mathematics became freed from such empiricism by fully embracing the
Non-Euclidean geometries, mathematicians started reconsidering ideas that had
been nagging at the edges of their theories for centuries. When Leibniz and
Newton independently invented calculus in the 17th century, they and their
contemporaries had to apply some intellectual gymnastics in regard to things
like the notions of limits and integrals. Calculus is all about considering the
final result of applying a given mathematical formula an infinite number of
times, or comparing values that differ only to an infinitessimal degree. But if
infinitessimal differences are real, and an infinitessimal is simply the
mathematical inverse of an infinite quantity... well, never mind. The consensus
from the 16th through the 19th centuries was that calculus was simply too
useful to ignore simply because it flirted with the idea that infinite
quantities must be legitimate mathematical objects.

::: info
Mathematicians had long before become accustomed to rationalizing abstract
mathematical entities in empirical terms. For example, negative numbers were
initially justified using reasoning based on what modern bookkeepers would
recognize as a double-entry ledger. If I owe you 3 dollars but only have 2
dollars, I will still owe you 1 dollar after giving you all I have. I.e. my net
holdings will be -1 dollars because as soon as I find another dollar I will be
obligated to hand it over to you. That sort of reasoning underlay how Western
mathematicians first embraced the concept of negative numbers.
:::

But starting in the late 19th century, enabled by the new focus on validity
rather than empirical truth, mathematicians like Cantor began to consider what
mathematics could say about things like infinite quantities. For one thing, is
there actually more than one infinite quantity? Naive intuition suggests that
there is not. After all, _$\infty$ + 1 = $\infty$_ is true in the same way that
_2 + 2 = 4_, assuming that $\infty$ is a thing in the first place. But if you
cannot change the value of $\infty$ through arithmetical operations, then there
is only one such infinite value that can be produced arithmetically.

But arithmetic is not all there is to mathematics. Let us define the set of
natural numbers, $\mathbb{N}$, as:

$$
\mathbb{N} = \{ x | x = 0 \lor \exists y (y \in \mathbb{N} \land x = y + 1) \}
$$

which is read in English as, "$\mathbb{N}$ is the set of all $x$'s such that
$x$ is zero or else there is a $y$ such that $y$ is in $\mathbb{N}$ and $x$ is
the result of adding $1$ to $y$." Cantor asked questions such as, "how many
elements are there in $\mathbb{N}$?" "Are there other infinite sets with a
different number of elements?" And so on.

Clearly, there are an infinite number of different infinite sets. For example,
note that $\mathbb{N}$, as defined above, contains only non-negative whole
numbers. The sets of integers, $\mathbb{I}$, and rational numbers,
$\mathbb{F}$, can be defined as:

$$
\begin{align}
\mathbb{I} = \{ x | x \in \mathbb{N} \lor \exists y (y \in \mathbb{N} \land x = -y) \} \\
\mathbb{F} = \{ x | x \in \mathbb{I} \lor \exists y, z (y \in \mathbb{I} \land x \in \mathbb{I} \land x = y / z) \}
\end{align}
$$

Just as clearly, the _cardinality_ (loosely meaning "the number of elements in
a given set" and denoted by enclosing the name of a set in vertical bars, e.g.
$|\mathbb{N}|$) of $\mathbb{N}$ is infinite since there is no limit to the
number of times you can add 1 to any given natural number. Naive intuition
suggests that the cardinality of $\mathbb{I}$ should be roughly twice as large
as the cardinality of $\mathbb{N}$ since the latter starts with all the
elements of $\mathbb{N}$ and then includes each natural number's negation. But
naive intuition also suggests the cardinalities of those two sets ought to be
the same because:

$$
\begin{align}
|\mathbb{N}| = \infty \\
|\mathbb{I}| = \infty \\
\infty = \infty
\end{align}
$$

Ditto for the relationship between $|\mathbb{N}|$ and $|\mathbb{F}|$. Naive
intuition suggests, on the one hand, that $|\mathbb{F}|$ should be roughly
$|\mathbb{N}|^2$ but, on the other hand, that both sets' cardinalities should
just be $\infty$.

Cantor presented proofs that the cardinalities of all three of the sets of
natural numbers, integers, and rational numbers are, in fact, the same (thus
initially appearing to validate naive intuition). He did so by showing that one
can produce a one-to-one mapping between elements of each of those sets.

In more formal terms, a _mapping_ between two sets is a third set, each of
whose elements is an ordered pair of elements from the first two sets. The
first element of each pair must be a member of the first set, called the
_domain_ of the mapping. The second element of each pair must be from the
second set, the _co-domain_ or _range_ of the mapping. Such a mapping is
_injective_ if it associates every member of the domain with exactly one member
of the range. It is _surjective_ if associates every member of the range with
exactly one element of the domain. It is _bijective_, or "one-to-one" if it is
both injective and surjective. Since a bijective mapping exhaustively matches
every element of both the domain and range with exactly one element of the
other, there must be exactly the same number of elements in both or else some
elements would have either been duplicated or left out of the mapping.

@graphviz_open
digraph G {

    node[shape=circle]

    subgraph injective {

        cluster=true;
        label="injective (into)";
        color=white;

        subgraph injective_domain {
            cluster=true;
            color=black;
            label="domain";

            ia[label="a"];
            ib[label="b"];

            ia -> ib[style=invis];
        }

        subgraph injective_range {
            cluster=true;
            label = "range";
            color=black;

            id[label="d"];
            ie[label="e"];
            if[label="f"];
            id -> ie[style=invis];
            ie -> if[style=invis];
        }

        ia -> id;
        ib -> ie;
    }

    subgraph surjective {

        cluster=true;
        label="surjective (onto)";
        color=white;

        subgraph surjective_domain {
            cluster=true;
            color=black;
            label="domain";
            sa[label="a"];
            sb[label="b"];
            sc[label="c"];

            sa -> sb[style=invis];
            sb -> sc[style=invis];
        }

        subgraph surjective_range {
            cluster=true;
            label = "range";
            color=black;

            sd[label="d"];
            se[label="e"];
            sd -> se[style=invis];
        }

        sa -> sd;
        sb -> se;
    }

    subgraph bijective {

        cluster=true;
        label="bijective (1:1)";
        color=white;

        subgraph bijective_domain {
            cluster=true;
            label="domain";
            color=black;

            ba[label="a"];
            bb[label="b"];
            bc[label="c"];

            ba -> bb[style=invis];
            bb -> bc[style=invis];
        }

        subgraph bijective_range {
            cluster=true;
            label = "co-domain";
            color=black;

            bd[label="d"];
            be[label="e"];
            bf[label="f"];
            bd -> be[style=invis];
            be -> bf[style=invis];
        }

        ba -> bd[dir=both];
        bb -> be[dir=both];
        bc -> bf[dir=both];
    }
}
@graphviz_close

Cantor's approach was to show that you can devise procedures for selecting
exactly one integer given any natural number, exactly one natural number given
any integer and that those procedures associate the same integer and natural
number which ever direction of the bijection you consider. Same for the natural
and rational numbers. So far, so good.

::: info
Any set for which there is an injective mapping from $\mathbb{N}$ is said to be
_countable_. If that set, itself, has an infinite cardinality then it is said
to be _countably infinite_. All countably infinite sets have the same
cardinality as $\mathbb{N}$.
:::

But what about the set of real numbers, $|\mathbb{R}|$? The first clue to there
being something different about $|\mathbb{R}|$ is that it isn't possible to
come up with a succinct definition of it similar to the definitions given above
for $|\mathbb{N}|$, $|\mathbb{I}|$ and $|\mathbb{F}|$.

Conventionally, the set of real numbers is defined loosely as "all the points
on the number line," also known as the _continuum_. Every point in the
continuum corresponds to a particular real a number, whose value is distinct
from the real number whose value is at some other point. The essential nature
of the continuum is that it is, well, _continuous_. Given any two points on the
number line, there are an infinite number of points between them. Take any two
of those "inner" points and there is, again, an infinite number of points
between those, as well.

Cantor showed that you cannot create a bijective mapping between $\mathbb{N}$
and $\mathbb{R}$. Even if you assume such a 1:1 mapping is possible between
$\mathbb{N}$ and just the points on the continuum between any two real numbers
(e.g. 0.0 to 1.0), there is a procedure by which you can always produce an
infinite number of real numbers that could not have been in the original
mapping. This means that all mapping between $\mathbb{N}$ and $\mathbb{R}$ are,
at best, injective and can never by surjective. Thus
$|\mathbb{N}|<|\mathbb{R}|$. Since both cardinalities are infinite, there are
at least to distinct infinite quantities which are orderable in the same way
that finite quantities are ordered. In fact, Cantor's arguments together with
subsequent work by Hilbert and others demonstrate that there are an infinite
number of such ordered infinite quantities, with $\mathbb{N}$ and $\mathbb{R}$
being the first two and (if assuming the axiom of choice) nothing in between
them. That latter assumption is known as the _continuum hypothesis_.

## Computability

Meanwhile, other philosophically inclined mathematicians and mathematically
inclined philosophers were working on other aspects of metamathematics. In
particular, the turn of the 20th century saw intense interest in the field of
formal linguistics: study of the nature of the kinds of symbolic languages used
to express mathematical formulas. One result of that work was to show that a
language with a finite number of distinct symbols and a finite number of rules
for putting those symbols together into syntactically correct formulas will
produce a countably infinite number of well-formed formulas, i.e. the set of
all possible calculations expressed as formulas of arithmetic, trigonometry,
calculus etc. all have the same cardinality as $\mathbb{N}$. But since that
cardinality is infinitely smaller than $|\mathbb{R}|$ there are,
therefore, an infinite number of real numbers that cannot be computed as the
result of any possible mathematical calculation. In fact, compared to the total
number of points the points on the continuum, the points that correspond to
computable numbers is so vanishingly small as to be statistically
insignificant.

Or put another way, every number that is ever used in calculation is a member
of a very special subset of all the points on the continuum. This begs the
question, what is so special about computable numbers that make them
computable? And can we actually say anything meaningful at all about the
transfinitely many more points on the continuum that are not computablity?

The answer to the second question is: not in the terms of any mathematical
language since then they would be computable. (More on this, below.)

Answering the first question, what makes a number computable in the first
place, is the subject of a branch of mathematics known -- you guessed it -- as
Computablity Theory. Computability Theory was pursued and fairly quickly
settled by Church and Turing (Church was Turing's dissertaion
advisor[<sup>*</sup>](#church)). Church's approach was entirely formal. He
invented a mathematical language called Lambda Calculus. His hypothesis was
that the set of computable numbers was the set of numbers that could be
calculated by evaluating all possible well-formed formuals of the Lambda
Calculus. Turing used thought experiments based on what he called a-machines
(it was Church who first called these Turning machines). Turing's hypothesis
was that the set of computable numbers was the set of outputs of all possible
such machines. Computability Theory was considered settled by what came to be
known as the Church-Turing Hypothesis when it was proven that the two
approaches yield the same set of numbers. I.e. the behavior of any possible
Turing machine can be represented as a formula of the Lambda Calculus and any
such formula can be used as the "blueprint" for such a machine.

::: info
This is what programming language theorists mean when they say a given language
is "Turing complete" -- but that is getting ahead in the story.
:::

### Computability Theory Begat Computer Science

When Church and Turing were working on Computability Theory in the 1930's, they
were pursuing the purist of metamathematics. It was not lost on themselves or
their contemporaries, however, that practical applications would quickly
follow. Turing machines are the conceptual model for programmable digital
computers and the Lambda Calculus is the forerunner of all programming
languages. The cleanest, clearest expression of the relationship between what
it means for a language to be "Turing complete" and the Lambda Calculus is the
Lisp family of languages, especially its ultimate apotheosis in the form of
Scheme.

For example, the following Scheme expression:

```scheme
((lambda (x) (+ x 1)) 2)
```

means exactly the same as the following expression of the lambda calculus:

$$
\lambda x . + \ x \ 1 \ ( 2
$$

Both represent the application of a function that adds 1 to its argument,
producing the value 3.[<sup>**</sup>](#lambda)

::: info
Yes, the term _lambda_ to mean an anonymous function as well as Amazon's choice
of _Lambda_ as the brand name of its "serverless" computing platform derives
from Church's Lambda Calculus and Lisp.
:::

## But Wait, There's More!

So, if not for a surprisingly intense interest in the parallel postulate over
the course of millenia, it might have taken mathematicians even longer than it
did to give themselves permission to pursue metamathematics as seriously, and
in many cases even more assiduously, as applied mathematics. This fostered an
interest in some areas of "pure" mathematics (a term which was coined only
after the need arose to distinguish metamathematics from traditional,
empiricist mathematics when regarded as a science). Among those new areas of
interest were set theory, formal linguistics and computability theory. The
results from such metamathematical research immediately gave rise to Computer
Science. They also provided tools that helped physicists develop General
Relativity and Quantum Mechanics among other revolutionary discoveries of the
20th century.

Returning to the fundamental questions underlying Computability Theory, the
Church - Turing Hypothesis answers the question of how to characterize
computable real numbers, at least from a purely formal perspective. It says
absolutely nothing about all those vastly more points  on the number line
representing numbers that are not computable. I.e. it says nothing qaulitative
about numbers, computable or otherwise. This still leaves us with the question
of what can we say about non-computable numbers?

The answer is, of course, that there is nothing we can say about non-computable
real numbers because they only exist as a side-effect of an intuitively
appealing, historically widely held understanding of the nature of "number"
that has no correspondance with any other aspect of mathematics or science.
I.e. it is simply a mistake, post Cantor, Church and Turing, to continue to
regard non-computable real numbers as being at all "real" or even "numbers."

These putative quantities only exist as a consequence of the assumption that
there is such a thing as the infinitely subdividable continuum. But such a
thing cannot exist in the real world. Consider a piece of rope. You could cut
it in the middle to produce two lengths of rope, each of which was half the
size of the original. You could cut one of those in half to produce two pieces,
each of which was a quarter of the original. But in the real world you can
repeat that process only so many times before you reach a point where the
results after cutting are so small as to have lost any resemblance to "lengths
of rope" simply because of the physical materials out of which the rope was
made and how those materials are combined to form a "length of rope". Laying
such concerns aside, if you choose to continue the subdividing process on the
bits of fiber that are the tiny remnants of what is no longer a rope, you will
reach a point where you are pulling apart microscopic bits of composite
materials into mono-molecular chunks. Keep going and you will eventually reach
a point where you are separating molecules into the their constituent elemental
atoms. Continue, and you will start pulling atoms apart into clouds of
electrons, protons and neutrons. Keep at it, and you will see vast numbers of
every species in the "particle zoo" until you arrive at some final level
(branes, according to M-theory) whose discreteness and indivisability,
presumably, give rise to the quanta of Quantum Physics. I.e. modern science has
amply confirmed that the physical world, at the most fundamental level, does
not behave like a continuum.

Given that physical reality, the continuum can at best be one of those
artifacts of a purely formal mathematical theory, like the "completed
infinities" that were once anathema and now seem commonplace among mathematical
theories. But from that point of view, Cantor's proof that
$|\mathbb{N}|<|\mathbb{R}|$ can just as easily be understood to be a _reductio
ad absurdum_ argument that the continuum cannot exist even as a theoretical
artifact of pure number theory. From that point of view, the set of computable
numbers represents the exhaustive list of all possible numbers and is countably
infinite. For this reason, mathematical texts should just stop talking about
"real numbers" as a set distinct from the computable ones.

---

<a id="church"></a><sup>*</sup> Decades later, the author of this document took
classes from Church while he was still teaching, so I have that in common with
Turing. :-)

<a id="lambda"></a><sup>**</sup> The expression of the Lambda Calculus is
written in the informal syntax Church used when writing on chalk boards (using
actual chalk on actual slate-covered boards) in university classrooms. He was a
big fan of prefix notation (sometimes referred to as Polish notation because it
was first popularized by a particular group of logicians from Poland). One
advantage of prefix notation is that it greatly reduces the need for grouping
symbols like parentheses (which is why only a left parenthesis appears here).
This is ironic because the main reason that Lisp never really caught on as a
programming language outside of computer science classrooms and AI labs was due
to its over-reliance on parentheses in a syntax that many programmers find
off-putting.
