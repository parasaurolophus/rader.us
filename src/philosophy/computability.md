# How the Parallel Postulate Changed the World

<!-- toc -->

## Abstract

Mathematicians from Euclid's time to the start of the modern era were strangely obsessed with the fifth postulate from his treatise on two-dimensional Geometry, [Elements](https://en.wikipedia.org/wiki/Euclid%27s_Elements). Over the course of millenia there were countless attempts to prove whether the fifth, a.k.a. parallel postulate was really an axiom or, as was suspected for a long time but eventually disproven, actually a theorem hiding simpler axioms under its petticoats. The solution to this question precipitated a revolution in mathematical theory and practice that ultimately birthed the post-modern "information age" as well as contributing to scientific and engineering advances across many disciplines, including General Relativity and Quantum Mechanics. No, really.

## Euclid's Fifth Postulate

Once upon a time, mathematicians in the Western tradition regarded themselves as engaged in a science, not different in kind from chemistry or physics. In this way of regarding mathematical truth, the fact that \\( 2 + 2 = 4 \\) is proven by empirical observation: if I hold two pebbles in my hand then pick up two more, I will be holding a total of four pebbles. This empiricism was used not only to justify claims about which mathematical statements were true and which were not, it guided the way that mathematical problems were posed and solved. As mathematical theories advanced and techniques expanded to include an ever increasing degree of abstraction, they remained for a very long time rooted in empiricism. Negative numbers, for example, were initially justified by using the kind of reasoning modern bookkeepers would recognize as a double-entry ledger.

Euclid's _Elements_ epitomizes this view of the nature and practice of mathematics. While providing the model for an axiomitic approach to mathematical proof, his actual method was firmly based in "constructions," which are empirically verifiable operations using a physical straight-edge and compass. Euclid's axioms and postulates all hearken directly to such empiricism. They are _nearly_ all very simple declarative statements of easily obsverable facts or simple steps in a construction. "To extend a point to form a line," "to extend a line to form a plane," and so on.

Then there is the fifth postulate, often referred to as the parallel postulate. It looks nothing like the others. In Euclid's original formulation, it is a nearly paragraph-sized sentence rather than a short phrase and can be difficult to puzzle out without aid of a diagram. Even later attempts to produce simpler formulations did not detract from the degree to which it seems like an outlier compared to the other axioms and postulates.

> Euclid's fifth postulate:
>
> If a line segment intersects two straight lines forming two
> interior angles on the same side that are less than two right
> angles, then the two lines, if extended indefinitely, meet on
> that side on which the angles sum to less than two right angles.

![](fifth_postulate_diagram.png)

From Euclid's time right into the start of the modern era, mathematicians were not happy about this. Over the course of millenia, countless attempts were made to show that the parallel postulate must actually be a theorem, derivable from simpler axioms that look more like the others in Euclid's book. While this produced a number of alternative formulations of the axiom, some of which may be easier to visualize than others, none were a reduction to radically simpler axioms. For example:

> Alternative formulation of the fifth postulate:
>
> Given a line and a point not on that line, there is exactly
> one line that can be drawn through the given point that
> does not meet the given line when both lines are
> extended indefinitely in both directions.

![](alternate_fifth_postulate_diagram.gif)

After enough time and failed attempts, Western mathematicians eventually shifted their focus away from trying to prove that the parallel postulate was really a theorem. Instead they tried to form an understanding of why it is actually an axiom. By the early 19th Century, mathematical techniques had evolved to the point that allowed the following kind of indirect proof to be carried out:

1. Assume some different axiom that contradicts Euclid's parallel postulate in some specific way.
2. Show that the version of Euclid's constructions and proofs that can be carried out using this different version of the parallel postulate are just as _internally consistent_ as Euclid's "classical" proofs.

If the second step of the preceding proof can be accomplished for one or more variations from the parallel postulate assumed in the first step, then this proves that Euclid's fifth postulate was an axiom all along. If it were not an axiom but a theorem, altering it would emerge as a contradiction among the more basic axioms from which it is derived.

The key here is to separate the idea of "internal consistency" from "empirical truth." The so-called "Non-Eudclidean Geometries" invented by the likes of [Bolyai](https://en.wikipedia.org/wiki/J%C3%A1nos_Bolyai), [Lobachevsky](https://en.wikipedia.org/wiki/Nikolai_Lobachevsky), [Riemann](https://en.wikipedia.org/wiki/Bernhard_Riemann) _et al._ each produce different theorems that contradict those described by Euclid, but as long as such Non-Euclidean axioms do not contradict themselves for a given version of the parallel postulate, they demonstrate that each such variation on the theme of "Geometry" is just as good, in some sense, as every other &mdash; including Euclid's own. Carrying out proofs in Riemann's [Elliptic Geometry](https://en.wikipedia.org/wiki/Elliptic_geometry) is like doing Euclid-style constructions on the surface of a spheroid where all lines forming "great circles" around the circumference will eventually intersect. Lobachevsky's [Hyperbolic Geometry](https://en.wikipedia.org/wiki/Hyperbolic_geometry) assumes, on the other hand, that you can draw more than one line through any point that do not intersect some other line. This is harder to visualize than Elliptic Geometry. In this case it is like doing Euclid-style constructions on a surface with a hyperbolic curvature. Neither variation corresponds to what happens when, like Euclid, you use a straight-edge and compass on a flat surface. But neither Lobachevskian nor Riemannian Geometries produce _self_-contradictory results. And so Euclid's fifth postulate was demonstrated to have been an axiom all along.

Mathematicians of their day rejoiced at Bolyai's, Lobachevsky's and Riemann's accomplishments for multiple reasons. They could finally regard the whole question pertaining to the status of the parallel postulate as satisfactorily settled. They could start playing with the new mathematical techiques developed for this enterprise, seeking additional areas of mathematical exploration where they might apply. But there was a reckoning to be had at the end of the celebration: in order to accept the highly desirable result, mathematicians were forced to alter fundamentally their understanding of their own profession. Mathematicians became concerned with _validity_ (internal consistency) rather than (empirical) _truth_. Euclid's theorems do not merely form an interesting set of mutually consistent mathematical formulas. Euclid's reliance on "constructions" demonstrate that his theorems describe properties of objects and operations carried out in the real world. Non-Eudclidean Geometries have no such correspondence to observable reality (at least not at the scale at which human senses operate). Not only does every valid Non-Euclidean Geometry contradict Euclid's "common sense" Geometry, each contradicts every other. But if validity is the goal rather than correspondence to empirical truth, that does not matter at all. A "mathematical truth" was accepted as proven even though that required abandoning the need for, or ability to, assert anything at all about empirical truth solely on the basis of mathematics.

## Formal Linguistics, Set Theory and Infinite Quantities

The change in outlook from truth to validity opened the way to vast new realms of mathematical enquiry. Suddenly, the nature of "validity," itself, became a subject of study along with an examination of the nature of axiomatic systems and the formal languages used to define and apply them when carrying out mathematical proofs. The detachment from empiricism also gave permission for mathematicians to consider subjects that had formerly been forbidden because they referred to mathematical objects and states of affairs that could not exist in the real world.

For example, right up to the middle of the 19th Century it was considered "out of bounds" for mathematicians to posit the existence of "completed infinities" in their calculations. After all, you can never hold an infinite number of pebbles in your hand. When Leibniz and Newton separately invented Calculus in the 17th Century, they each had to perform some mental gymnastics that allowed them to treat "infinitessimal" quantities as mathematically sound while still shying away from the mathematical inverses of infinitessimals, i.e. infinite quatities. As a foreshadowing of how the world of science and mathematics would react to Non-Euclidean Geometry a couple of centuries later, Leibniz' and Newton's contemporaries found Calculus too compelling and useful to ignore, while consciously repressing the cognitive dissonance required to embrace infinitessimals. That repression finally ended when the generation of mathematicians following the invention of Non-Euclidean Geometries fully embraced the consequences: go ahead and construct proofs based on infinite quantities or any other formerly forbidden category of mathematical object. So long as your results are internally consistent then your mathematics are valid. There may or may not be a "meta-mathematical theory" mapping your results to properties of the real world, as in the case of Euclidean Geometry. If so, that may be a serendipitous side-effect of potential use to scientists and engineers, but such a corresondence to reality is no concern of "pure" math in this new way of looking at things.

Simultaneously, the new emphasis on validity begged the question: what exactly is "validity," in the first place? Previously, it had been considered both necessary and sufficient to demonstrate the correctness of a mathematical theory to show how it described properties of real things. Even abstrations like negative numbers were justified by treating them the way that bookkeepers treat balance sheets. Oddities like what happens when you try to divide any number by zero were regarded as special cases needing no further explanation than, "that can't happen in the real world, so just skip over it." But once approaches like those which used Non-Euclidean Geometries to prove something about classical Geometry, such empiricism was no longer necessary nor sufficient. The nature of mathematics and mathematical formulas in and of themselves suddenly became a topic of great interest to mathematicians. Thus were Symbolic Logic, Set Theory and Formal Linguistics born as areas of intense mathematical inquiry. Symbolic Logic is a mathematical system for determining whether or not a set of statements are mutually consistent, i.e. form a valid argument. Set Theory is an extension of Symbolic Logic that considers the mathematical properties of collections of "things" without regard to the properties of the things themselves. Formal Linguistics is the study of the properties of the sets of symbols and rules for combining them used to express mathematical formulas (or, later, computer programs) in meaningful and useful ways.

[Cantor](https://en.wikipedia.org/wiki/Georg_Cantor) used this new freedom and these novel approaches to consider the following question: is there more than one infinite quantity? If Cantor had been born even a generation earlier, he would have been laughed out of his doctoral program for even posing such a question. Infinite quantities were "right out" (to quote Monty Python) as mathematical objects in the first place, let alone the question of what mathematical properties they might have. But by Cantor's day, the world of academic mathematics was ready to give almost anything a go.

The school-yard understanding of "infinity" is that once you reach it, that's all there is. If a grade-schooler says to her best friend, "I love you," the friend may reply, "I love you more!" The natural response is, "I love you twice as much!" Which elicits, "I love you times 10!" Then "...times 100!" Eventually, one of the amicable combatants will end the game with, "...times infinity," to which, on the school yeard at least, there is no retort since (as every child understands) once you reach "infinity" you cannot count any higher. Cantor understood this school-yard intuition as meaning that what most people think of as \\(\infty\\) corresponds to the cardinality of the set of natural numbers. As a Set Theoretician would say, when you count up indefinitely from 1, you are defining the set of natural numbers by intention (as opposed to defining a set by extension, which would require an explicit list of all its members).

To understand Set Theory's terminology like "cardinality," "intension vs. extension" and so on, consider the letters of the Latin alphabet as a set of symbols. There are twenty-six of them as used in English, so the cardinality of this set is 26. "Cardinality" can be loosely understood as "the number of members of a given set." However, "cardinality" is used to allow the notion to be extended to sets with infinitely many members. Historically, people were hesitant to refer to infinite quantities as "numbers," so a different term was needed for "number of members" when that number was infinite. The set of "letters of the alphabet" is defined by extension, as demonstrated by the "...now I've learned my A, B, C's..." chant every American grade-schooler is taught. I.e. what makes `{A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z}` the set of "letters of the alphabet" is simply the assertion that the given list _is_ the given set. This is what it means to define a set by extension. Only finite sets can be defined by extension, because only a finite number of items can be listed explicitly.

But what about a set like the natural numbers? Prior to Cantor's day, the answer would have been, "there is no such thing as the set of natural numbers because that would represent a completed infinity." But the new interest in things like Set Theory and the new freedom to explore valid theories whether or not they could be empirically verified allows one to define "the set of natural numbers" by _intension_. That is to define rules by which anything can be tested to determine whether or not it belongs in the set. So the set of natural numbers can be defined by intension using a rule.

> Definition of the set of natural numbers by intention:
>
> Anything is a natural number if and only if it is the number 1
> (or 0, if you prefer) or it is the result of adding 1 to a
> natural number.

By the preceding rule, 2 is in the set of natural numbers because it is the result of 1 + 1. Given that, so is 3, because it is the result of 2 + 1. And so on. The number 1.5 is not a natural number because there is no way to produce it by adding 1 to any natural number. This allows "the set of natural numbers" to be well defined, but what about its cardinality? Cantor begins his exploration of infinite quantity simply by stipulating that the set of natural numbers when defined by intention has a cardinality, usually denoted using the last letter of the Greek alphabet, \\(\Omega\\). Since there is no limit to the number of times you can add 1 to a natural number, \\(\Omega\\) represents an infinite quantity. But is \\(\Omega\\) "all there is" when it comes to infinite quantities, as in the school-yard intuition regarding \\(\infty\\)?

Cantor's answer, as it turns out, is, "no, there are an infinite number of infinite quantities which differ in magnitude in the same way as the magnitudes of finite numbers differ." To prove this, we need to intoduce some additional jargon: "mapping" and "countability." A "mapping" is a rule by which elements in one set are associated with elements in another set. For finite sets defined by extension, such mappings can be defined by extension as well. A mapping can be represented as a set of ordered pairs where the first member of a pair is from one set and the second member of a pair is from the other set. An obvious mapping from natural numbers to letters of the Latin alphabet as used in English would be:

```
 1, A
 2, B
 3, C
 4, D
 5, E
 6, F
 7, G
 8, H
 9, I
10, J
11, K
12, L
13, M
14, N
15, O
16, P
17, Q
18, R
19, S
20, T
21, U
22, V
23, W
24, X
25, Y
26, Z
```

This is simply labeling each letter with its position in the conventional ordering when reciting the "A, B, C" song, which is also the normal sorting order when alphabetizing entries in a telephone directory, a book's index or the like. Another name for such a mapping is a "function" where the set from which the first member of each pair is selected is the "domain" and the set from which the second member of each pair is selected is called the "range." The significance of the "function" terminology will become apparent when discussing such mappings between sets with infinite cardinalities. Note that this particular mapping has some interesting properties, making it "one to one" (1:1): every element in each set corresponds to exactly one element in the other such that no element in either set is left out of the mapping and no element in either set corresponds to more than one element in the other. Only sets with the same cardinality ("number of elements") can have 1:1 mappings between them.

As with infinite sets themselves, mappings involving them must be defined using a rule for choosing an element from the range given an element from the domain. Where both sets consist of numbers, an obvious way of defining such a rule is as a mathematical formula, hence the use of the term "function" for such mappings. If such a mapping is 1:1, this means that the rule (function) is such that given a particular element from the domain, it will choose (compute) exactly one element from the range and it would be possible to define an inverse rule (function) choosing (computing) exactly one element in the domain given a particular element in the range. If such a 1:1 mapping is possible then the two infinite sets have the same cardinality. Any set for which a 1:1 mapping is possible between that set and the set of natural numbers is said to be "countable" since one could "count" the elements in the given set by computing the element of that set that corresponds to any given natural number using the 1:1 function, i.e. producing an infinitely long list of pairs that looks similar to the one above for the 26 letters of the alphabet. The cardinality of every countable set is \\(\Omega\\).

For Cantor, the test of the school-yard intuition that once you reach \\(\infty\\), that is as big as a quantity can be thus became: is the cardinality of every infinite set equal to \\(\Omega\\), i.e. are all infinite sets countable? His answer was that while some infinite sets are countable, others are not. See <https://en.wikipedia.org/wiki/Cantor%27s_diagonal_argument> for a description of his arugment that the set of real numbers is not countable. To summarize it briefly, assume that you have a countable set of real numbers between any two points on the number line. It is possible to apply a procedure for generating an infinite sequence of digits representing a real number that cannot be in your original list. Therefore there are (infinitely) more real numbers between any two points on the number line than there are natural numbers, no matter how close together you choose the end points of the segment of the number line to start with. If we accept Cantor's argument then the cardinality of the set of real numbers, \\(|\mathbb{R}|\\), is infinitely larger than \\(\Omega\\), the cardinality of the set of natural numbers.

## The Computability Problem

As stated above, the definition of a countable set is that there is a 1:1 function that maps natural numbers to elements of that set. If the set of real numbers is not countable, that means that there are real numbers for which no such function is possible. Infinitely more. In fact, a consequence of Cantor's arguments is that the percentage of real numbers for which such a mapping function is possible is so small compared to the real numbers denoted by all the points on the number line, the ones that do correspond to a countable set must be very special in some way. And so a new branch of mathematics was born, Computability Theory.

To understand Computability Theory, you first must know something about Formal Linguistics. As already noted, Formal Linguistics is the study of the kinds of artifical languages used to express mathematical formulas (and, some time after Cantor, computer programs). One of the most famous results in Formal Linguistics is [G&ouml;del's](https://en.wikipedia.org/wiki/Kurt_G%C3%B6del) "incompleteness theorems." One of the things proven along the way is that the number of well-formed formulas for any language of the kind used for mathematical purposes is countable. This means that one can re-frame the understanding that "the set of real numbers is not countable" as "most real numbers are not computable &mdash; i.e. there is no formula for computing their values". This is because their are only countably many formulas and there infinitely more real numbers than natural numbers. Computability Theory attempts to answer the question, "what makes a real numbers computable?"

## Comutability Theory to the Rescue

But how does one go about characterizing what distinguishes a computable number from the vastly larger population of real numbers, generally? (Leaving aside, for now, the question of how "real" a number can be if it cannot be computed.) Starting in the early 20th Century, various philosophers of mathematics and philosophically-inclined mathematicians worked on this question. The question was considered settled when the work of two such people, [Church](https://en.wikipedia.org/wiki/Alonzo_Church) and [Turing](https://en.wikipedia.org/wiki/Alan_Turing), converged (Church was Turing's Doctoral advisor).

Church's approach was purely formal. He invented a mathematical language, the [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus), for defining any possible calculation. Church's thesis was that the set of computable numbers is the set of results of evaluating all well-formed formulas of the Lambda Calculus. This way of characterizing computability is very much in keeping with what had by then become the mainstream "all that matters is validity" view of mathematics.

Turing's approach starts with thought experiments involving hypothetical "[Turing Machines](https://en.wikipedia.org/wiki/Turing_machine)." (Turing called these "a-machines" in his dissertation; it was Church who invented the term "Turing Machine.") In this way, Turing's work actually fits well with the classical, "empiricist" view of mathematics. A Turing Machine is a theoretical mechanical device built according to certain rules and which produces a particular sequence of symbols (e.g. digits in the representation of a real number) as the result of its operation. If Turing Machines could be constructed (which they could not be in reality because each would require an infinitely long paper tape), Turing's thesis is that the outputs of all possible such machines would be the set of computable numbers.

Computability Theory was considered settled and the world moved on to other interests when it was shown that Church's and Turing's theses are mathematically equivalent: the behavior of any possible Turing Machine can be described using a formula of the Lambda Calculus, and any formula of the Lambda Calculus can be used as the "blueprint" for a Turing Machine. As a side-effect, Turing and Church had between them provided the mathematical model and mental framework for programmable digital computers and for the programming languages used to control them. But before we delve further into the ongoing legacy of their work, note something interesting: neither Church's nor Turing's work actually directly addresses what makes computable numbers different from all those other supposedly-real-but-not-computable ones. Instead, they provide a means for characterizing an exhaustive set of computable numbers while saying nothing at all about non-computable ones, other than by implicitly excluding them.

In fact, rather than explaining Cantor's results, Computability Theory when considered on its own seems to imply that the set of real numbers actually is countable using some ordering of the formulas of the Lambda Calculus to drive the 1:1 mapping function. [That such an ordering is possible is another consequence of G&ouml;del's work.](https://en.wikipedia.org/wiki/G%C3%B6del_numbering) Cantor's argument is interesting and valid, but the success of Computability Theory begs the question: how "real" is a number that can't be computed? That there are such tensions between "pure" and "applied" mathematics became inevitable once mathematicians embraced validity over truth. What is remarkable is, despite that tension, how often what begins as an exploration of a pure mathematical abstraction, e.g. Computability Theory, yields results with powerful and even world-changing applications, e.g. Computer Science. Similar examples can be found in other scientific domains. Non-Euclidean Geometries have found practical use in various branches of Physics rather than remaining the province of abstract considerations of Euclid's axioms.

To return to Computability Theory, programmers use the term "lambda" to refer to an anonymous function because of Church's Lambda Calculus. Turing Machines were the original model for the [Von Neumann Architecture](https://en.wikipedia.org/wiki/Von_Neumann_architecture) on which all current programmable electronic computing devices are based. Conversely, Cantor's non-computable real numbers can only ever remain the stuff of abstract considerations of the theoretical properties of an infinitely subdivisable number line. That said, the same embrace of infinite quantities Cantor pioneered has been useful in both abstract and applied math in much more concrete ways. Division by zero, which once was regarded as an anomalous discontinuity, now is understood to have a definite result: \\(\pm\infty\\). That is,

\\[\lim_{n \to 0}{1 \over n} = \infty\\]

since it is simply the inverse of

\\[\lim_{n \to \infty}{1 \over n} = 0\\]

(The preceding is baked into the [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) standard for floating-point arithmetic, along with the special `NaN` value for remaining cases of undefined operations such as \\(0 \over 0\\), attempting to calculate the square root of a negative number and so on.)