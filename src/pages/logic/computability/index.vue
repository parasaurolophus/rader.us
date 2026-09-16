<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>
        <h1>Computablity Theory</h1>

        <p class="subtitle">Or, How Not-So-Real Numbers Changed Everything</p>

        <p>
            Computer Science, as an academic discipline is a direct outgrowth of
            earlier work in a fairly obscure branch of pure mathematics known as
            <i>Computability Theory</i>. The primary work on computability was
            carried out in the 1930's by Alonzo Church in the United States and
            Alan Turing in the United Kingdom. Between them, they not only
            resolved the original, central question of what makes a number
            computable, as an unintended side effect they invented the formal
            mechanisms defining a programming language and a programmable device
            on which to run it. Referring to anonymous functions in any programming
            language as lambdas" is a direct reference to Church's
            <i>&lambda;-Calculus</i>. Talking about electronic computing devices
            as "machines" and the basic set of operations such a device can
            perform as "machine language" is a direct reference to Turing's
            work.
        </p>

        <h2>From Euclid to Cantor</h2>

        <p>
            To understand the underlying problem which Church and Turing were
            trying to solve through their work on computability requires delving
            into a very long succession of mathematical cononundrums which
            begins, like so much else, in ancient Greece. Here is a very
            abbreviated version of this history.
        </p>

        <p>
            From the time at which Euclid's <i>Elements</i> was first written
            onwards, mathematicians in the Islamic and Western European
            tradition were slightly obsessed with the fifth of Euclid's axioms
            of Geometry, which came to be known as the <i>parallel postulate</i>
            (despite the word "parallel" appearing nowhere in Euclid's original
            version of it.) The reason that the fifth axiom caused so much
            discussion is that it looks and sounds nothing like the preceding
            four axioms. While the first four axioms are very short, obvious
            declarative sentences (at least when translated from the ancient
            Greek dialect used in Euclid's text into modern terminology):
        </p>

        <ol>
            <li>
                Any two points can be joined by a straight line.
            </li>
            <li>
                Any line can be extended indefinitely from either of its ends.
            </li>
            <li>
                Any line can be rotated around either of its ends to form a
                circle.
            </li>
            <li>
                All right (i.e. 90°) angles are equal.
            </li>
        </ol>

        <p>
            The first three of the preceding axioms refer to the basic
            operations of geometric construction using a straight-edge and
            compass. The fourth is simply a special case, applied to right
            angles, of the general principle that things with identical
            properties are identical to each other.
        </p>

        <p>
            The fifth postulate, on the other hand, is much wordier and requires
            more thought to work out what it is even saying.
        </p>

        <ol start="5">
            <li>
                If two lines that cross a given a line are extended
                indefinitely, the two lines will eventually intersect on the
                side of the given one where the angles formed by their
                intersection with the given one add up to less than the sum of
                two right angles.
            </li>
        </ol>

        <p>
            Most people find a diagram helpful:
        </p>

        <ParallelPostulate />

        <p>
            When visualized that way, the fifth axiom seems as intuitively valid
            as the first four. But it still seems like a very different kind of
            beast. So different, in fact, that starting in Euclid's time and for
            millenia onward there were numerous attempts to either show that it
            was actually a theorem hiding some set of simpler axioms under its
            skirts or else find some way of replacing it with another axiom
            whose style is more in keeping with the other four. The most famous
            example of the latter approach is Playfair's axiom, which can be
            expressed in contemporary English as:
        </p>

        <blockquote>
            Given a line and a point not on that line, at most one line can be
            drawn through the point that does not intersect the given line.

            <i>John Playfair</i>
        </blockquote>

        <p>
            Playfair showed that the phrase "at most one line" is sufficient to
            serve as a replacement for Euclid's parallel postulate when taken
            together with the other axioms and "postulates" of planar geometry.
            In fact, the stronger "exactly one line" is true, among the many
            theorems that can be proven whether using Euclid's or Playfair's
            version of the fifth axiom.
        </p>

        <p>
            What does any of this have to do with Computability Theory, let
            alone Computer Science? The history of mathematics consists of
            literally thousands of years' worth of successive, incremental
            refinement of earlier theories. Questions that were first posed in
            the 3rd Century BCE were still being considered in the 19th Century
            CE. Along the way, the tools and modes of thought mathematicians
            employed when tackling these kinds of questions were utterly
            transformed. In Euclid's time, Mathematics was considered to be a
            science not different in kind from Physics or Chemistry. That
            <math>
                <mrow>
                    <mn>2</mn>
                    <mo>+</mo>
                    <mn>2</mn>
                </mrow>
                <mo>=</mo>
                <mn>4</mn>
            </math>
            was understood in classical times as nothing different from the fact
            that if you hold two pebbles in your hand and then pick up two more,
            you will be holding four pebbles. Even though Euclid's
            <i>Elements</i> is considered to be one of the earliest known
            examples of a truly axiomatic treatment of a branch of Mathematics,
            the approach taken is firmly rooted in this empiricist world view.
            Hence its first three axioms' fundamental reliance on
            "constructions" using straight-edge and compass in the method of
            proving many of its theorems.
        </p>

        <aside>
            At the time, the kinds of "Non-Euclidean Geometries" pioneered by
            Lobachevsky and Riemann were considered to be abstractions that did
            not correspond with "reality" in the way that Euclid's, apparently,
            does (at least when drawing figures on a flat sheet of paper). As
            physicists used the kinds of mathematical tools afforded by these
            new approaches to form theories related to phenomena outside the
            scale at which human senses operate, they discovered that when
            considered at the cosmologically enormous scale of General
            Relativity or the ultra-microscopic scale of Quantum Mechanics, the
            real world often presents features better described in non-Euclidean
            terms. More on this, later...
        </aside>

        <p>
            By the 1800's, empiricism was seen as a limitation to a mathematical
            theory rather than its grounding and ultimate source of truth.
            Attempts like Playfair's to come up with "better" axioms than
            Euclid's inspired even more audacious projects. In particular,
            Nicolai Lobachevsky and Bernhard Riemann went beyond Playfair in not
            just creating alternative versions of the axioms of planar geometry
            that could produce an equivalent set of theorems to Euclid's.
            Lobachevsky and Riemann, working indepently, showed (among many
            other things) that it was possible to define an infinite number of
            alternative Geometries, each of which produced different sets of
            theorems from the others, but where each was just as internally
            consistent with itself as Euclid's.
        </p>

        <p>
            This helped crystalize a change that had already been under way for
            some while. From that time onward, the focus of Mathematics shifted
            firmly and finally away from empirical truth to logical validity. In
            order to embrace the mathematically powerful and useful results of
            Lobachevsky's and Riemann's theories, mathematicians had to renounce
            any requirement for a mathematical structure to correspond to
            observable phenomena. From a 19th Century perspective, while only
            Euclidean Geometry was "true" in terms of constructions performable
            in the real world, all of the literally boundless number of
            alternative non-Euclidean Geometries are equally "valid" in the
            sense of each producing its own unique set of provably consistent
            theorems.
        </p>

        <p>
            One effect of this shift of focus was to introduce the concept
            of <i>Metamathematics</i>. Metamathematics posits a conceptual
            mapping from particular Mathematical theories to some set of
            observable features of objects in the real world. I.e. while all
            Geometries are equally valid from an abstract Mathematical point of
            view, it is a Metamathetical theory that gives Euclidean a special
            status as also being a true description of figures drawn on a flat
            sheet of paper using straigt-edge and compass. This notion is not
            specific to Geometry. Number Theory is the study of <i>groups</i>
            and <i>fields</i> with features which make them similar to
            Arithmetic, while Arithmetic enjoys the metamathematical status of
            corresponding to what happens when you count actual objects in the
            real world. And so on across all the branches of Mathematics.
        </p>

        <aside>
            Leibnitz' and Newton's arguments denying infinite quantities were,
            of course, poppycock. If you accept that your theory acknowledges
            the existence of infinitessimal differences it thereby acknowledges
            the existence of infinite differences, as well. The one is simply
            the algebraic inverse of the other:

            <math display="block">
                <mrow>
                    <mrow>
                        <munder>
                            <mo>lim</mo>
                            <mrow>
                                <mi>x</mi>
                                <mo>&rightarrow;</mo>
                                <mn>&infin;</mn>
                            </mrow>
                        </munder>
                        <mo>(</mo>
                        <mfrac>
                            <mn>1</mn>
                            <mi>x</mi>
                        </mfrac>
                        <mo>)</mo>
                    </mrow>
                    <mo>=</mo>
                    <mn>0</mn>
                </mrow>
                <mo>&equiv;</mo>
                <mrow>
                    <mrow>
                        <munder>
                            <mo>lim</mo>
                            <mrow>
                                <mi>x</mi>
                                <mo>&rightarrow;</mo>
                                <mn>0</mn>
                            </mrow>
                        </munder>
                        <mo>(</mo>
                        <mfrac>
                            <mn>1</mn>
                            <mi>x</mi>
                        </mfrac>
                        <mo>)</mo>
                    </mrow>
                    <mo>=</mo>
                    <mn>&infin;</mn>
                </mrow>
            </math>

            Again, much more on this, below...
        </aside>

        <p>
            Which brings us to Georg Cantor and Set Theory. From time
            immemorial, most mathematicians in the tradition stretching back to
            ancient Greece were dismissive of the concept of a "completed
            infinity" &mdash; i.e. a set with an infinite number of members. One
            can never, after all, hold an infinite number of pebbles in one's
            hands. Even Leibnitz and Newton who, centuries before Cantor,
            independently invented Differential Calculus, expended a fair amount
            of intellectual effort attempting to show that even though the
            concepts of <i>limits</i> and <i>derivatives</i> are all about
            infinitessimal differences in magnitude this somehow did not imply
            the existence of infinite quantities. After the breakthroughs by
            Lobachevsky's and Riemann's generation of mathematicians, subsequent
            generations were liberated to pursue mathematical inquiries divorced
            from Metamathematical constraints. Cantor, in particular, explored
            the properties of infinite sets &mdash; simply stipulating that they
            exist in the first place, at least for the purposes of "pure"
            mathematics.
        </p>

        <p>
            The first challenge Cantor faced was to define what the "size" of a
            set with an infinite number of members even means. Cantor asserted
            that every set has a <i>cardinality</i>. The cardinality of a set is
            often denoted by surrounding the name of the set with magnitude
            bars, such as
            <math>
                <mo>|</mo>
                <mi>A</mi>
                <mo>|</mo>
            </math>.
        </p>
        <p>
            For sets with only a finite number of members, the notion is
            intuitive:
        </p>

        <math display="block">
            <mtable>
                <mtr>
                    <mtd>
                        <mrow>
                            <mo>|</mo>
                            <mi>A</mi>
                            <mo>|</mo>
                        </mrow>
                        <mo>=</mo>
                        <mn>3</mn>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mtext>where</mtext>
                        <mspace width="0.5em" />
                        <mrow>
                            <mi>A</mi>
                            <mo>=</mo>
                            <mrow>
                                <mo>{</mo>
                                <mi>a</mi>
                                <mo>,</mo>
                                <mi>b</mi>
                                <mo>,</mo>
                                <mi>c</mi>
                                <mo>}</mo>
                            </mrow>
                        </mrow>
                    </mtd>
                </mtr>
            </mtable>
        </math>

        <p>
            I.e. the cardinality of a set with three elements is 3. The
            definition of cardinality for sets with infinitely many members is
            trickier. Sets like
            <math>
                <mi>A</mi>
            </math>, above, can be defined by <i>extension</i>, in other words
            by listing their elements. Since no such explicit list is possible
            for an infinite set it must be defined by <i>intention</i>, i.e. by
            stating a rule by which its members can be identified iteratively.
            The set of natural numbers,
            <math>
                <mi>&naturals;</mi>
            </math>, is defined:
        </p>

        <math display="block">
            <mi>&naturals;</mi>
            <mo>=</mo>
            <mrow>
                <mo>{</mo>
                <mi>n</mi>
                <mo>:</mo>
                <mrow>
                    <mo>(</mo>
                    <mi>n</mi>
                    <mo>=</mo>
                    <mn>0</mn>
                    <mo>)</mo>
                    <mo>&or;</mo>
                    <mrow>
                        <mo>&exist;</mo>
                        <mi>m</mi>
                        <mo>(</mo>
                        <mo>(</mo>
                        <mi>m</mi>
                        <mo>&in;</mo>
                        <mi>&naturals;</mi>
                        <mo>)</mo>
                        <mo>&and;</mo>
                        <mrow>
                            <mo>(</mo>
                            <mi>n</mi>
                            <mo>=</mo>
                            <mi>m</mi>
                            <mo>+</mo>
                            <mn>1</mn>
                            <mo>)</mo>
                        </mrow>
                        <mo>)</mo>
                    </mrow>
                </mrow>
                <mo>}</mo>
            </mrow>
        </math>

        <p>
            Informally, <math>
                <mi>&naturals;</mi>
            </math> is the set consisting
            of 0 and every number that can be obtained by adding 1 to another
            member of <math>
                <mi>&naturals;</mi>
            </math>. Historically,
            mathematicians would have denied the legitimacy of referring to
            <math>
                <mi>&naturals;</mi>
            </math> as a set in the first place, let
            alone consider how many elements it contains. They would, at best,
            have said that the limit of indefinitely carrying out the successor function that
            is the intention of <math>
                <mi>&naturals;</mi>
            </math> might be
            denoted <math>
                <mn>&infin;</mn>
            </math>, while hastening to disclaim
            <math>
                <mn>&infin;</mn>
            </math> as being a number in the same way that
            any particular natural number represents a specific quantity. Thanks
            to the shift away from empiricism, it had finally become tolerable
            by Cantor's day to explore the theoretical ramifications of sets of
            axioms for which there were no obvious metamathematical theory. He
            reasoned that if <math>
                <mi>&naturals;</mi>
            </math> is a set, it must
            make sense to talk about
            <math>
                <mo>|</mo>
                <mi>&naturals;</mi>
                <mo>|</mo>
            </math> without regard to whether or not that cardinality
            corresponded to any finite number. To do so, he reasoned by analogy
            to finite sets.
        </p>

        <p>
            Returning to the previously defined set,
            <math>
                <mi>A</mi>
            </math>, with cardinality 3, consider how that cardinality compares
            to other finite sets. If
            <math>
                <mi>B</mi>
                <mo>=</mo>
                <mrow>
                    <mo>{</mo>
                    <mi>d</mi>
                    <mo>,</mo>
                    <mi>e</mi>
                    <mo>,</mo>
                    <mi>f</mi>
                    <mo>}</mo>
                </mrow>
            </math> then
            <math>
                <mrow>
                    <mo>|</mo>
                    <mi>A</mi>
                    <mo>|</mo>
                </mrow>
                <mo>=</mo>
                <mrow>
                    <mo>|</mo>
                    <mi>B</mi>
                    <mo>|</mo>
                </mrow>
                <mo>=</mo>
                <mn>3</mn>
            </math>. Further, note that if the cardinalities of two finite sets
            are equal then it is possible to construct a <i>bijective
                function</i>, a 1:1 mapping, between their respective members. One
            example of such a mapping for
            <math>
                <mi>A</mi>
            </math> and
            <math>
                <mi>B</mi>
            </math> is:
        </p>

        <math display="block">
            <mo>{</mo>
            <mrow>
                <mo>&lt;</mo>
                <mi>a</mi>
                <mo>,</mo>
                <mi>d</mi>
                <mo>&gt;</mo>
            </mrow>
            <mo>,</mo>
            <mrow>
                <mo>&lt;</mo>
                <mi>b</mi>
                <mo>,</mo>
                <mi>e</mi>
                <mo>&gt;</mo>
            </mrow>
            <mo>,</mo>
            <mrow>
                <mo>&lt;</mo>
                <mi>c</mi>
                <mo>,</mo>
                <mi>f</mi>
                <mo>&gt;</mo>
            </mrow>
            <mo>}</mo>
        </math>

        <p>
            Cantor extended that reasoning to infinite cardinalities. He
            asserted that two sets have the same cardinality if it is possible
            to construct a bijective mapping between their elements without
            regard to whether their cardinalities are finite or infinite.
        </p>

        <p>
            Other kinds of mappings between elements of sets imply other
            relationships between cardinalities. If it is possible to construct
            a surjective mapping from one set to another, the first set's
            cardinality must be greater than or equal to that of the second set.
            Conversely, if it is possible to construct an injective mapping, the
            first set's cardinality must be less than or equal to the second
            set's. Finally, if it can be shown that an injective or surjective
            mapping is possible but a bijective one is not, then the two
            cardinalities cannot be equal.
        </p>

        <p>
            This implies the counter-intuitive idea that if infinite sets are
            allowed by a mathematical theory to exist, then there is at least
            the possibility of their being an infinite number of different
            infinite quantities which can be ordered in the same way that the
            elements of <math>
                <mi>&naturals;</mi>
            </math> can be ordered. Cantor
            went on to show that, indeed, according to the traditional notion of
            <i>real numbers</i> existing on a <i>continuum</i>,
            <math>
                <mi>&reals;</mi>
            </math>, any mapping from
            <math>
                <mi>&naturals;</mi>
            </math> to
            <math>
                <mi>&reals;</mi>
            </math>
            can be, at best, injective. Therefore,
            <math>
                <mrow>
                    <mo>|</mo>
                    <mi>&naturals;</mi>
                    <mo>|</mo>
                </mrow>
                <mo>&lt;</mo>
                <mrow>
                    <mo>|</mo>
                    <mi>&reals;</mi>
                    <mo>|</mo>
                </mrow>
            </math>. This further implies that, since other theorems of Set
            Theory state that there can be no "greatest" cardinality just as
            there can be no greatest natural number (no matter how high you have
            counted up, you can always count up one more), there are not just
            two distinct infinite cardinalities but infinitely many,
            well-ordered cardinalities. Cantor referred to this ordered set of
            infinite cardinalities as the set of <i>ordinal numbers</i> with the
            cardinality of
            <math>
                <mi>&naturals;</mi>
            </math>
            being the least ordinal number in the same way that 0 is the least
            natural number. He used "aleph numbers" to denote ordinal numbers
            where
            <math>
                <msub>
                    <mn>&aleph;</mn>
                    <mn>0</mn>
                </msub>
            </math>, corresponding to the cadinality of
            <math>
                <mi>&naturals;</mi>
            </math>, is the first ordinal, the next higher ordinal is
            <math>
                <msub>
                    <mn>&aleph;</mn>
                    <mn>1</mn>
                </msub>
            </math> and so on. The <i>continuum hypothesis</i> in Set Theory
            states that there is no set with a cardinality greater than that of
            <math>
                <mi>&naturals;</mi>
            </math> and less than that of
            <math>
                <mi>&reals;</mi>
            </math>, making the cardianlity of
            <math>
                <mi>&reals;</mi>
            </math> equal to
            <math>
                <msub>
                    <mn>&aleph;</mn>
                    <mn>1</mn>
                </msub>
            </math>.
        </p>

        <p>
            Cantor presented multiple proofs that the cardinality of
            <math>
                <mn>&naturals;</mn>
            </math> is less than that of
            <math>
                <mn>&reals;</mn>
            </math>. The easiest to visualize is a <i>reductio ad absurdum</i>
            argument showing that starting with the assumption that you have
            already constructed a bijective mapping between the members of
            <math>
                <mi>&naturals;</mi>
            </math> and any continuous subset of
            <math>
                <mi>&reals;</mi>
            </math> you can always construct another real
            number within the continuous subset that cannot yet have been mapped
            by your supposedly bijective function. This proves that the mapping
            was only injective, after all.
        </p>

        <h2>How Real Is the Real Number Line?</h2>

        <p>
            This brings us, at last, to Computability Theory. Since the
            cardinalities of the sets of natural and real numbers are both
            infinite, the only way you can say anything meaningful about the
            relationship between them is through inductively defined mappings.
            But Cantor showed that, given the traditional definition of what it
            means for the set of real numbers to be "continuous" (in the way
            that the set of natural numbers is <i>not</i> continuous)
            deductively proves that any inductively defined mapping from
            <math>
                <mi>&naturals;</mi>
            </math> to any continuous subset of
            <math>
                <mi>&reals;</mi>
            </math>
            will leave out infintely many real numbers, no matter how tiny the
            difference is between the least and greatest member of the
            continuous subset of real numbers. In the language of Set Theory,
            any continuous subset of
            <math>
                <mn>&reals;</mn>
            </math> is not
            <i>countable</i>, where to be "countable" means a bijective mapping
            is possible between members of a given set and the members of
            <math>
                <mi>&naturals;</mi>
            </math>.
        </p>

        <p>
            But here's the thing. Building on Cantor's work, other
            mathematicians (principally Kurt G&ouml;del but others, as well),
            showed that any formal language of the kinds used by mathematicians
            to perform computations can have at most a countable number of
            well-formed formulas. I.e. any language whose grammar consists of a
            finite number of rules for combining a finite number of symbols into
            syntactically correct and meaningful formulas can produce no more
            such formulas than there are members of
            <math>
                <mi>&naturals;</mi>
            </math>. Together with the proof that even
            a tiny continuous subset of <math>
                <mi>&reals;</mi>
            </math> is not
            countable, this means that the percentage of real numbers that can
            be computed, i.e. characterized by some mathematical formula, is
            infinitessimally small compared to the cardinality of
            <math>
                <mi>&reals;</mi>
            </math> as a whole. If you take the notion of
            <i>continuum</i> seriously, then only a negligible fraction of real
            numbers corresponds to numbers about which math or science has
            anything to say.
        </p>

        <p>
            This begs the question, what's so special about those precious few
            real numbers that makes their values computable? Church, Turing and
            their contemporaries tackled the question by finding ways to
            construct the set of computable numbers comparable to the way in
            which a set with infinite cardinality can be defined by intention.
            I.e. by describing a method for generating numbers that can produce
            at least a countable number of outputs. Their theses were that the
            set of numbers produced by their respective methods is each
            identical to the set of computable numbers. Note that there is a
            circularity to any claim that Church or Turing proved that their
            methods produce the set of computable numbers. What they actually
            did was to provide models of computation that each produce a
            countable number of values. The claim that the given countable set
            of values is identical to the computable real numbers can only be a
            (highly plausible!) appeal to intuition. Mathematicians generally
            accepted the core question of Computability Theory to have been
            settled by the fact that Church's and Turing's approaches each
            produce the same set of values, as described below.
        </p>

        <p>
            Church's approach was purely formal. He defined a mathematical
            language, &lambda;-Calculus for describing computations. Church's
            thesis was that the set of computable numbers is identical to the
            values of all possible well-formed &lambda; formulas. Turing's
            approach was based on thought experiments involving what he called
            <i>a-machines</i> (it was Church who first started referring to them
            as "Turing machines.") Turing's thesis was that the set of outputs
            of all possible a-machines is identical to the set of computable
            numbers. What came to be known as the Church-Turing Thesis is that
            their two approaches are equivalent, i.e. they produce the same set
            of computable numbers. The world accepted that the initial question
            of Computability Theory was settled when the Church-Turing Thesis
            was proven to be true. The proof of the Church-Turing Thesis turns
            on showing that any well-formed formula of &lambda;-Calculus can be
            used as the "blueprint" for constructing an equivalent a-machine,
            and vice versa; i.e. the rules governing any a-machine can be
            "encoded" in the form of a well-formed formula of &lambda;-Calculus.
        </p>

        <aside>
            While FORTRAN is widely credited as being the first programming
            language to be used for real-world applications, work on Lisp at MIT
            actually started earlier and has had a much more profound and
            lasting influence on the design of programming languages, generally.
            Lisp began as a project to "embody" &lambda;-Calculus directly as a
            practical programming language in exactly the same way that modern
            digital computers "embody" a-machines.
        </aside>

        <p>
            This gave mathematicians something to say about the abstract notion
            of "computability." It also gave engineers a way of thinking about
            the design of physical computational aids that went far beyond the
            then-current state-of-the-art. You can never build an actual Turing
            machine because you would need an infinite amount of data storage
            (an infinitely long paper tape in Turing's thought experiments). But
            it is trivial to use Turing's conceptual framework for a
            general-purpose computing device to create an actual digital
            computer capable of tackling computations with a finite number of
            steps, requiring only a finite number of digts of precision.
            Similarly, Church's &lambda;-Calculus provided the prototype for
            designing high-level programming languages, making it possible to
            think about programming in ways that would be extremely challenging
            when operating only at the level of a given machine's base
            instruction set, modeled on Turing's a-machines.
        </p>

        <p>
            Backing up to what Cantor actually proved about infinite
            cardinalities and how that shaped the Church-Turing Thesis, it must
            be emphaized that while Cantor's arguments are perfectly valid, that
            says nothing about any metamathematical implications for measurable
            features of the real world. General Relativity and Quantum Physics
            each have been treated as implying some very counter-intuitive, if
            not outrageously implausible, things. And they don't even agree on
            their implausibilities, at least according to the average
            popular-science level of discussion about them. But mystification
            about modern Physics' putative contradiction of common-sense is
            mostly based on very bad metaphysics and metamathematics. Nothing
            should be <em>less</em> surprising than that, for example, things
            like billiard balls are not made of tinier things that behave just
            like tinier billiard balls caroming off one another. It is easy to
            forget that when operating outside the scale at which human senses
            operate, and therefore with which our habits of thought evolved to
            cope, we are <em>always</em> reasoning by analogy and abstraction.
        </p>

        <p>
            A pile of gears, springs, levers and such has no properties by which
            to tell time even when the contents of that pile could be used as
            parts from which to construct a perfectly serviceable chronometer.
            That is a metaphysical insight worthy of the first day in any
            undergraduate course on Metaphysics. So why is it so hard to fathom
            that subatomic particles, constituent parts of actual waves and
            particles of matter, are extremely unlikely to themselves be
            particles or waves even when they can exhibit some features that
            make them appear a bit wave-like when analyzed one way and other
            features that make them appear a bit particle-like when analyzed
            another way? It would be more startling, suggesting some kind of
            infinitely reductive regression, if it turned out that macroscopic
            waves and particles were made of microscopic waves and particles.
            This is similar to the "mind-body" promblem that once consomed so
            much of Western European Philospers' time and effort. In trying to
            make sense of how something non-material could interact and control
            a material body, some philosophers resorted to this kind of
            fruitless reductionism. Descartes, for example, seemed serious in
            suggesting that the pineal gland, of all things, was where the soul
            resided and was the central command center from which it exerted
            control over the body, using the rest of the brain as an
            intermediary. As if localizing the mind to a tiny
            brain-witnin-a-brain actually did anything to resolve the
            "mind-body" problem in the first place. The only real way to resolve
            the "mind-body" problem is, of course, to recognize that the entire
            question is based on a false premise. The question disappears
            entirely when one regards mental function as an epiphenomenon of
            neural function. But that was a move Descartes, as a devout
            Catholic, could not allow himself to make since it eliminates
            anything like a "soul," existing separately and potentially
            independently of a body, from consideration. Physicists should
            simply free themselves of the superstition that reasoning by too
            much by analogy from everyday experience in a domain where classical
            physics makes sense when doing physics at ultra-microscopic scales
            can lead to anything but confusion and blind alleys. Ditto when
            reasoning by analogy at the cosmological scale. Double ditto for the
            supposed contradictions reasoning by analogy between the subatomic
            and cosmological scales. Physicists would do well to stick to
            Physics and leave Metaphysics to the professionals in the Philosophy
            department.
        </p>

        <p>
            This applies to all the supposedly mind-bending properties of matter
            and energy when considered at the quantum level. Physicists choose
            to forget, or were never taught, that the entire edifice of advanced
            Mathematics on which their discipline relies depends on mathematical
            tools with very specific metamathematical foundations. Cantor's
            proofs, and Church's and Turing's subsequent insights, can be viewed
            as perfectly valid proofs of why the continuum is not a countable
            set of numbers. But note well: they do so by simply stipulating that
            the continuum exists in the first place. Quantum Physics, on the
            other hand, demonstrates that reality is not continuous when viewed
            at ultra small scales. The best way to look at Cantor's proofs from
            a strictly scientific, rather than mathematical, point of view is as
            higlighting limits (pun intended) to the utility of the notion of a
            continuum from the outset. Physicists should recalibrate their
            thinking by taking their own results seriously. Re-working the
            metamathematical framework used by theoretical physicists could have
            far-reaching effects. Differential Calculus and mathematical
            structures that rely on it are ideal for the purposes of Classical
            Mechanics (for which they were designed in the first place) but
            should only be used for Quantum Mechanics with extreme care, if at
            all. Fully eschewing the notion of "continuity" could help resolve
            puzzling aspects of modern physics like entanglement and whether or
            not indeterminacy really has a "tail problem." A measuable property
            is, by definition, computable. If not, one could not design an
            experiment to take the measurement in the first place. Might the
            gaps in the continuum left between computable numbers be big enough
            to conceal some hidden variables? No-one knows, at the time of this
            writing, but it seems sufficiently plausible to temper the
            enthusiasm for the (slightly circular, to start with) claims about
            qubits in quantum computers offering confirmation that quantum
            effects in the real world display Bell inequalities of the sort that
            suppoedly clinch the argument in favor of "spooky action at a
            distance."
        </p>

    </div>

</template>

<script setup lang="ts">
import ParallelPostulate from '@/pages/logic/computability/ParallelPostulate.vue'
</script>