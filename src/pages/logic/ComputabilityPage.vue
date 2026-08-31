<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <h1>Computability Theory and the History of Computer Science</h1>

        <h2>or, Not-So-Real Numbers and How They Changed the World</h2>

        <p>
            A pet peeve of mathematicians in the Islamic and Western European
            tradition for much of the last two millenia revolved around Euclid's
            fifth postulate from his <i>Elements</i>, a. k. a. the
            <i>parallel postulate</i>. For those needing a reminder from their
            middle school geometry classes, here is Thomas Heath's translation
            of Euclid's five axioms of planar geometry, <i>ca.</i> 1908:
        </p>

        <blockquote>

            <p>
                Let the following be postulated:
            </p>

            <ol>

                <li>
                    To draw a straight line from any point to any point.
                </li>

                <li>
                    To produce a finite straight line continuously in a straight
                    line.
                </li>

                <li>
                    To describe a circle with any centre and distance.
                </li>

                <li>
                    That all right angles are equal to one another.
                </li>

                <li>
                    That, if a straight line falling on two straight lines make
                    the interior angles on the same side less than two right
                    angles, the two straight lines, if produced indefinitely,
                    meet on that side on which are the angles less than the two
                    right angles.
                </li>

            </ol>

        </blockquote>

        <p>
            The first three of these axioms simply and obviously refer to the
            ability to perform constructions using a straight-edge and compass.
            The fourth axiom is an equally simple special case of Euclid's more
            general "common notions" regarding self-similarity and the fact that
            things that are equal to the same thing are equal to each other, as
            applied to 90&deg; angles. The fifth axiom needs explaining.
        </p>

        <p>
            It is striking how the first four axioms are all very short,
            declarative statements (at least when transformed into more modern,
            idiomatic sentences) that are easy to understand and visualize. By
            contrast, the fifth postulate looks more like a paragraph's worth of
            obscure description involving multiple lines and angles. What it
            ultimately boils down to is every bit as intuitively true as the
            first four once you work out what it actually says, but getting
            there for most people requires a diagram:
        </p>

        <div class="columns">

            <img src="/logic/parallel-postulate.svg">

            <div>
                The fifth postulate states that the intersection point
                <math>
                    <mi>P</mi>
                </math>
                is on the same side of
                <math>
                    <mi>L</mi>
                </math>
                as angles
                <math>
                    <mi>a</mi>
                </math>
                and
                <math>
                    <mi>b</mi>
                </math>
                when both angles are less than 90&deg;
            </div>

        </div>

        <p>
            From Euclid's own time right through to nearly the present day there
            was unease regarding the fifth postulate. At first, the thought was
            that since it seemed so much more complex than the other four, it
            might actually be a theorem hiding additional, simpler axioms under
            its skirts. Over the course of centuries, many attempts to identify
            such simpler axioms were made that only succeeded in producing
            variations in wording, or logically equivalent axioms that were not
            actually much less complex. The most famous such variant is John
            Playfair's <i>"given a line and a point not on it, at most one line
                parallel to the given line can be drawn through the point."</i>
        </p>

        <p>
            Eventually, it was widely accepted that the fifth postulate is,
            indeed, an axiom required for the rest of Euclid's Geometry to be
            proven. The focus shifted to proving that fact, in hopes to shed
            light on why it was so different from the other four axioms while
            remaining an axiom, itself. It was not until the 19th century --
            over two thousand years after Euclid wrote his <i>Elements</i> --
            that the mathematical tools had been developed to accomplish this.
            As has often happened in the history of mathematics, once
            mathematicians generally had embraced certain techniques, multiple
            people developed similar theories at more or less the same time with
            lots of contentiousness over who got there first if not outright
            accusations of plagiarism. The hostility between Newton and Leibnitz
            and their respective followers over the invention of Calculus is the
            most famous example. A less well known, but far more brutal example
            was that of Bernhard Riemann and Nikolai Lobachevsky. Working
            independently, both of them proved that alternative versions of
            Geometry can be created by changing the parallel postulate. Among
            other interesting results, this proved definitively that the
            parallel postulate was itself an axiom all along by showing that by
            changing it you could derive a different set of theorems than those
            described by Euclid but that were, themselves, just as mutually
            consistent among themselves. (Riemann's incredibly influential
            mentor, Carl Gauss, was so incensed by the idea of his star pupil's
            having to share credit with Lobachevsky thet he embarked on a smear
            campaign dismissing Lobachevsky as at best an imitator and at worst
            a plagiarist. This was part of the inspiration for Tom Lehrer's
            comic song from the 1950's,
            <a href="https://en.wikipedia.org/wiki/Lobachevsky_(song)" target="_blank">Lobachevsky</a>. The reality is
            that Non-Euclidean
            Geometry was an idea whose time had come and it was inevitable that
            multiple mathematicians would be working on it simultaneously
            without necessarily knowing of one another's existence.)
        </p>

        <p>
            At the time, this was a radical idea. Up until then, most
            mathematicians regarded themselves as scientists working in a field
            not substantively different from Physics or Chemistry. From this
            point of view, there is no difference between logical validity and
            empirical truth. After all, that
            <math>
                <mn>2</mn>
                <mo>&plus;</mo>
                <mn>2</mn>
                <mo>&equals;</mo>
                <mn>4</mn>
            </math>
            can be demonstrated by holding two pebbles in your hand, then
            picking up two more. Euclid's first three postulates strongly biased
            mathematicians toward this kind of empiricism since they are
            expressed in a way to show that you can demonstrate his theorem's by
            drawing pictures in the real world on a (flat) sheet of paper.
            Geometry was Geometry, no qualifiers needed, precisely because of
            Euclid's practice of using such empirical demonstrations in his
            method of proving theorems.
        </p>

        <aside>
            Interestingly, one generation's purely abstract mathematics can
            become the next's highly useful metamathematical tool. It turns out
            that when considering the nature of reality at very large
            (cosmological) or very small (sub-atomic) scales, non-Euclidean
            geometries can provide better descriptions of reality than Euclid's
            planar geometry. In fact, Riemann's hyberbolic geometry directly
            paved the way for Einstein's theory of General Relativity.
        </aside>

        <p>
            Riemann and Lobachevsky proved that Euclid's fifth postulate is an
            axiom by starting with the same other four axioms while modifying
            the fifth axiom. They then showed that one could derive a set of
            theorems using their respective fifth axioms that contradicted
            Euclid's (and each other's) but which contained no contradictions
            amongst themselves.
        </p>

        <p>
            In order to embrace this result as settling a long-standing
            mathematical conundrum, mathematicians had to re-think their whole
            field of endeavor. Euclidean, Riemannian and Lobachevskian
            Geometries are all equally valid, but only one (Eculidean) is
            empirically true (when operating on a plane, at human scale). To say
            that Riemann's and Lobachevsky's approach settled the question of
            the parallel postulate was to admit that Mathematics is not a
            science but, rather, a set of techniques for exploring the logical
            consistency of sets of theorems independent of empirical truth. The
            term <i>metamathematics</i> was coined to refer to a separate theory
            by which certain mathematical systems, like Euclidean Geometry,
            correspond to features of the real world, like the actual properties
            of real shapes drawn on flat pieves of actual paper, in ways useful
            to scientists and engineers.
        </p>

        <p>
            While many mathematicians initially regarded this as an affront,
            most quickly realized that this actually opened up whole realms of
            inquiry that previously had been closed off precisely because they
            had no empirical basis. In particular, from the time of the ancient
            Greeks onward, Western mathematicians had resolutely refused to
            acknowledge infinite quantities as an area for legitimate
            mathematics. One can never hold an infinite number of pebbles in
            your hand. (Even Newton and Leibnitz, whose ground-breaking work had
            relied on the notion of quantities that only infinitesimally differ
            in magnitude, had gone to great lengths to disavow that their
            theories required that infinite quantities be accepted as real even
            though, obviously, they did since the simple arithmetic inverse of
            <math>
                <mfrac>
                    <mn>1</mn>
                    <mn>&infin;</mn>
                </mfrac>
            </math>
            is
            <math>
                <mn>&infin;</mn>
            </math> such that if you accept the former as being mathematically
            legitimate then it is illegitimate to deny the legitimacy of the
            latter.)
        </p>

        <p>
            With "validity" as the essential goal of mathematics rather than
            "truth," mathematicians were freed to pursue concepts like infinite
            quantities with abandon. One such mathematician, Georg Cantor,
            showed that if any infinite quantity exists, then there are
            infinitely many infinite quantities that differ from one another in
            the same way that finite quantities differ. For example, he proved
            that if "the continuum" is assumed to exits, i.e. the set of all
            real numbers exists in the sense traditionally understood as an
            infinitely sub-dividable "number line," then there must be more real
            numbers between any two real numbers, no matter how small the
            difference between them, than there are members of the set of all
            natural numbers even though both are infinitely large sets. I.e. it
            isn't just that the cardinality of <i>&reals;</i>, the set of all
            real numbers, is larger than the cardinality of the set of natural
            numbers, <i>&naturals;</i>. the cardinality of the subset of
            <i>&reals;</i> represented by any finite segment of the real number
            line, no matter how tiny, is larger than the cardinality of the
            entirety of <i>&naturals;</i>.
        </p>

        <p>
            Cantor showed this by way of a <i>reductio ad absurdum</i> argument
            beginning with the assumption that it is possible to define a 1:1
            mapping between the members of <i>&naturals;</i> and some
            arbitrarily chosen subset of <i>&reals;</i>, such as all the real
            numbers between 0 and 1. If such a mapping were possible, then every
            natural number would correspond to a particular sequence of digits
            representing every real number beginning with "0." according to the
            usual conventions for representing real numbers in the US and UK:
        </p>

        <math display="block">
            <mtable>
                <mtr>
                    <mtd>
                        <mn>0</mn>
                        <mtext>:</mtext>
                    </mtd>
                    <mtd>
                        <mn>0.437</mn>
                        <mtext>&hellip;</mtext>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mn>1</mn>
                        <mtext>:</mtext>
                    </mtd>
                    <mtd>
                        <mn>0.928</mn>
                        <mtext>&hellip;</mtext>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mn>2</mn>
                        <mtext>:</mtext>
                    </mtd>
                    <mtd>
                        <mn>0.053</mn>
                        <mtext>&hellip;</mtext>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mtext>&vellip;</mtext>
                    </mtd>
                </mtr>
            </mtable>
        </math>

        <p>
            Since the preceding mapping is assumed for the sake of argument to
            be 1:1, it must contain every possible sequence of digits starting
            with "0." But Cantor pointed out that there is a simple procedure
            for generating a sequence that could not possibly be in the mapping.
            For the first digit following "." in the new sequence, simply choose
            any digit other the first digit after "." in the sequence that maps
            from 0. For the next digit, choose any digit other than the second
            digit after "." that maps from 1. This procedure can be repeated
            indefinitely, choosing any digit other than the <i>n</i>'th digit in
            in the corresponding row of the mapping. This procedure would
            produce a sequence of digits corresponding to a real number that
            differs in at least one digit from every entry in the mapping. This
            contradicts the assumption that the mapping was 1:1 in the first
            place, completing the <i>reductio ad absurdum</i> proof by showing
            that there is an unmapped real number, also proving that the
            cardinality of the subset of <i>&reals;</i> between 0 and 1 is at
            least 1 greater than the cardinality of <i>&naturals;</i>.
        </p>

        <p>
            Further, note that for any set of sequences of digits, you can
            repeat indefinitely the preceding process to keep generating new
            sequences that could not be in the given set. So the cardinality of
            the subset of <i>&reals;</i> between 0 and 1 is not just 1 greater
            than the cardinality of <i>&naturals;</i>, it is infinitely greater.
            If that is true for the arbitrarily chosen subset of real numbers
            between 0 and 1, the same is true for the real numbers between 0 and
            0.5, 0.25 and so on. In fact, it is true for every subset of
            <i>&reals;</i> defined by a minimum and maximum value, no matter how
            small the difference between them.
        </p>

        <math>
            <mtable>
                <mtr>
                    <mtd>
                        <mo>&forall;</mo>
                        <mi>x</mi>
                        <mo>,</mo>
                        <mi>y</mi>
                        <mo>(</mo>
                        <mo>&verbar;</mo>
                        <mrow>
                            <mi>S</mi>
                            <mrow>
                                <mo>(</mo>
                                <mi>x</mi>
                                <mo>,</mo>
                                <mo>y</mo>
                            </mrow>
                            <mo>)</mo>
                        </mrow>
                        <mo>&verbar;</mo>
                        <mo>&gt;</mo>
                        <mrow>
                            <mo>&verbar;</mo>
                            <mi>&naturals;</mi>
                            <mo>&verbar;</mo>
                        </mrow>
                        <mo>)</mo>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mtext>where</mtext>
                        <mspace width="0.5em" />
                        <mi>S</mi>
                        <mo>(</mo>
                        <mi>x</mi>
                        <mo>,</mo>
                        <mi>y</mi>
                        <mo>)</mo>
                        <mo>&equals;</mo>
                        <mo>{</mo>
                        <mrow>
                            <mi>r</mi>
                            <mo>&in;</mo>
                            <mi>&reals;</mi>
                        </mrow>
                        <mo>:</mo>
                        <mspace width="0.5em" />
                        <mrow>
                            <mi>x</mi>
                            <mo>&lt;</mo>
                            <mi>r</mi>
                        </mrow>
                        <mspace width="0.5em" />
                        <mtext>and</mtext>
                        <mspace width="0.5em" />
                        <mrow>
                            <mi>r</mi>
                            <mo>&lt;</mo>
                            <mi>y</mi>
                        </mrow>
                        <mo>}</mo>
                    </mtd>
                </mtr>
            </mtable>
        </math>

    </div>

    <p>
        A set's cardinality is said to be <i>countable</i> if it is no more than
        the cardinality of <i>&naturals;</i>. So Cantor's argument proves that
        <i>&reals;</i> is not countable. In fact,
        <math>
            <mo>&verbar;</mo>
            <mi>&reals;</mi>
            <mo>&verbar;</mo>
        </math> is
        trans-finitely greater than
        <math>
            <mo>&verbar;</mo>
            <mi>&naturals;</mi>
            <mo>&verbar;</mo>
        </math>. But consider the fact that the number of well-formed formulas
        of any language with a finite set of grammatical rules based on a finite
        character set is countable. E.g. the cardinality of the set of all
        grammatically correct expressions of a mathematical language like that
        of arithmetic is equal to
        <math>
            <mo>&verbar;</mo>
            <mi>&naturals;</mi>
            <mo>&verbar;</mo>
        </math>. This means that there are trans-finitely more real numbers than
        there are formulas of arithmetic that could be used to calculate their
        values. So, an immediate consequence of Cantor's proof is that only a
        trans-finitely small percentage of all real numbers are computable using
        the rules of mathematics. Another way of saying this is that, if talking
        about set like &naturals; and &reals; makes sense at all, then almost no
        member of &reals; corresponds to a number whose value can be calculated
        or characterized precisely in any way. Thus was born the short-lived
        branch of mathematics known as <i>Computability Theory</i>.
    </p>

    <p>
        Computability Theory explores the question of what is so special about
        that vanishingly small percentage of all real numbers that are
        computable. Its two leading practitioners were Alonzo Church and Alan
        Turing. (Church was Turing's thesis advisor and later correspondent as
        they each pursued Computability Theory independently.)
    </p>

    <p>
        Church took a formal approach. He invented a formal language called
        &lambda;-Calculus for representing calculations by way of the
        composition of arbitrarily defined functions. His hypothesis was that
        the set of computable numbers was the set of results of all well-formed
        formulas of the &lambda;-Calculus.
    </p>

    <p>
        Turing's approach was based on thought experiments. He described a kind
        of idealized device for performing a series of operations consisting of
        printing sequences of digits on infinitely long paper tapes. He called
        these hypothetical devices <i>a-machines</i> (<i>a</i> for
        <i>automatic</i>). It was Church who started calling them <i>Turing
            machines</i> when talking with colleagues and writing about them.
        Turing's hypothesis was that the set of computable numbers is identical
        to the set of outputs from all theoretically possible a-machines.
    </p>

    <p>
        The <i>Church-Turing Hypothesis</i> was that the set of results from all
        well-formed formulas of the &lambda;-Calculus is identical to the set of
        outputs from all a-machines. Mathematicians generally regarded the
        central question of Computability Theory to have been settled when the
        the Church-Turing Hypothesis was proven to be true. I.e. it is possible
        to design an a-machine to calculate the value of any &lambda;
        expression. It is also possible to represent the operations of any given
        a-machine as a &lambda; expression.
    </p>

    <p>
        An important thing to note about the Church-Turing Hypothesis is that
        a-machines are idealized devices, for which the only real-world
        limitation is that in order to output the exact value for real numbers
        like
        <math>
            <mfrac>
                <mn>1</mn>
                <mn>3</mn>
            </mfrac>
        </math>,
        <math>
            <mi>&pi;</mi>
        </math>, etc. the corresponding a-machine would need an infinitely long
        tape and infinite time in which to print digits. There is nothing
        stopping one from actually building in the real world any given
        a-machine so long as that particular machine requires only a finite
        number of operations to produce a value that can be represented using a
        finite number of digits like
        <math>
            <mrow>
                <mfrac>
                    <mn>1</mn>
                    <mn>2</mn>
                </mfrac>
            </mrow>
            <mo>&equals;</mo>
            <mn>0.5</mn>
        </math>.
    </p>

    <aside>
        The use of the term <i>lambda</i> to refer to anonymous functions in
        many programming languages and Computer Science texts is a direct
        reference to Church's &lambda;-Calculus.
    </aside>

    <p>
        Given that the operations carried out by any a-machine can be
        represented by an equivalent &lambda; expression, it is possible to
        describe a general-purpose a-machine which can be provided a &lambda;
        expression as input and produces the sequence of digits generated by
        evaluating the &lambda; expression as the a-machine's output. Between
        them, Turing and Church created the intellectual framework for the
        concept of a programmable digital computer and a programming language as
        a side-effect of their work on Computability Theory. In other words, the
        field of Computer Science owes its existence to Church, Turing and
        Computability Theory. In fact, Computer Science can be regarded as an
        applied sub-field of Computability Theory, restricted to a-machines with
        finite length inputs and outputs. Computability Theory, in turn, arose
        out of work on the nature of infinite sets and formal languages that was
        enabled by a chain of mathematical inquiries extending back to the
        ancient Greeks.
    </p>

    <h2>But Wait, There's More...</h2>

    <p>
        Returning to Cantor's proof regarding the cardinalities of &naturals;
        and &reals;, as already noted it depends on the assumption that "the set
        of real numbers" or, in fact, <em>any</em> set of real numbers
        corresponding to the traditional mathematicians' notion of "the
        continuum" makes sense. Given that assumption, it turns out that even
        though there are an infinite number of computable numbers, as a
        percentage of any portion of the continuum, the total number of
        computable numbers within that portion is infinitessimal. Though not
        often discussed in texts on Computability Theory and Computer Science,
        Cantor's <i>reductio ad absurdum</i> is best understood as disproving
        the idea of "the continuum" as a concept to start with. This makes the
        whole question underlying Computability Theory moot, since without the
        continuum there is no reason to account for non-computable numbers in
        the first place.
    </p>

    <p>
        This seemingly bold claim is justified by Cantor's own proof procedure.
        His process for showing that there can be no 1:1 mapping between
        &naturals; and any portion of the continuum does not actually apply only
        to such a mapping. It is actually a much stronger argument that no set
        which purports to include all real numbers within any range can actually
        do so as long as there is any ability whatsoever to apply a function to
        each of its members whether or not it is countable. If one accepts that
        any such ability to iterate by definition implies a mapping with the
        natural numbers, then the conventional interpretation of Cantor's
        results are fine. This is related to the axiom of choice, a mainstay of
        set theory, which provides the assumption that there is some way to
        create a unique mapping between &naturals; and the members of any
        countable set, even if (as is the case for the countable subset of any
        portion of the coninuum) there is no known rule for doing so explicitly.
        The fact that the axiom of choice is necessary to achieve certain
        significant results in set theory ought to give one pause about the very
        notion of uncountable sets to start with. To put it another way, if the
        axiom of choice reflects reality (for some definition of "reality") then
        there is no obvious reason that whatever unknown mechanism assigns
        values to corresponding elements of &naturals; could not continue to
        operate on the uncountable members of a set. This is, in fact, a
        necessary condition for the axiom of choice to apply to subsets of the
        continuum since it would not only need to know which elements to include
        in the mapping but be able to know to exclude members which would make
        the co-domain of the mapping uncountable. While one can always define a
        countable set of real numbers inductively by providing a rule for
        calculating a real number using some formula operating on the
        <i>n</i>'th natural number, Cantor's famous proof shows it is not really
        sensible to treat points along the continuum as a well-defined set to
        start with other than, at best, a purely mathematical abstraction of
        dubious utility even for mathematicians and philosphers. It only ever
        leads to tears, as with Zeno's Paradox and the like.
    </p>

    <p>
        Consider this from the point of view of metamathematics. If the
        continuum had any correspondence with actual matter in the real world,
        then it would be possible to create some object that would be infinitely
        subdividable into ever smaller and smaller pieces where each piece would
        still remain the same substance. Classical atomic theory shows this not
        to be true of actual matter. Repeatedly dividing any actual material
        into smaller pieces would eventually reach a point where molecules would
        be broken apart into atoms and then atoms into individual subatomic
        particles. Along the way, the individual pieces would stop being the
        same kind of material as that which you started out with. In addition,
        you would eventually reach a point at which the subdivision could not
        proceed (in particular, when the subdividing process had reached the
        point of breaking things like protons, neutrons and electrons into
        individual quarks, according to current particle physics mmodels at the
        time of this writing). Extending the argument to include quantum
        phenomena only pushes the level at which the subdivision stops to a
        somewhat deeper level of recursion. That is what the "quanta" of
        "quantum physics" refers to, after all.
    </p>

    <p>
        Quantum Physics is often treated with bafflement and wonder both by
        layman and even physicists because of how it seems to imply any number
        of counter-intuitive properties of the physical universe such as
        indeterminacy, super-position, entanglement etc. Some of that bafflement
        is due simply to a basic failure to take into account that all human
        reasoning involves a combination of abstraction, reduction and analogy
        such that it should actually be the opposite of surprising that things
        like billiard balls are not made up of smaller objects that behave
        exactly as if they were tiny billiard balls caroming off one another.
        But a good deal of the bafflement arises from the assumption that
        mathematical abstractions like the continuum might be expected to have
        any kind of real-world consequences, even when proofs like Cantor's show
        that they cannot, in principle, be relevant to any sensible
        metamathematical theory of the kinds employed by physicists.
    </p>

    <p>
        To put it bluntly, mathematicians and scientists should just stop
        treating mathematical abstractions like non-computable real numbers as
        if they were really real, because that way lies madness. Except, of
        course, when they are occasionally the impetus for world-changing events
        like ushering in the Information Age....
    </p>

</template>

<style scoped>
.columns {
    display: grid;
    grid-template-columns: max-content 20em;
}

.columns>* {
    align-self: center;
}
</style>