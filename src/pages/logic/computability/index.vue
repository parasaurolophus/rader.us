<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div id="container">

        <div id="computability">

            <h1>Computability</h1>

            <p>
                <strong>
                    ...or why most real numbers are not as real as you think,
                    and yet still managed to change everything.
                </strong>
            </p>

            <p>
                Once upon a time, there was brief but intense interest in a branch
                of mathematics called Computability Theory. The essential question
                of Computability Theory is, "What makes a number computable?" That
                question arises based on the recognition that if you take the
                notion of <a href="#continuum">the continuum</a> seriously &mdash;
                as is required, for example, for mathematical notions like
                <i>limits</i> and <i>derivatives</i> to make sense &mdash; then
                there exists for only an infinitessimal subset of all the real
                numbers any mathematical formula that can be used to compute
                their values. This turns out to be case because:
            </p>

            <ol>

                <li>
                    The number of points on the continuum between any two
                    points is not <a href="#countable">countable</a>.
                </li>

                <li>
                    There are only a countable number of formulas of any
                    mathematical language that can be used to perform
                    computations.
                </li>

            </ol>

            <p>
                The first of the preceding claims, that the <a href="#cardinality">cardinality</a> of the set of real
                numbers,
                ℝ, is not countable, was shown by Georg Cantor and others in the
                late 19th century. The second was shown in the early 20th
                century by Kurt Gödel and others. Specifically, any formal
                language with a finite number of rules for combining a finite
                number of symbols into well-formed formulas can produce only a
                countable number of such formulas, so any mapping from
                mathematical forumals to any continuous set of real numbers can
                be at best <a href="#injective">injective</a> and never <a href="#surjective">surjective</a>.
            </p>

            <p>
                Taken together, the two observations about the relative
                cardinalities of the sets of real numbers and the number of
                formulas available to compute their values, shows that almost no
                point on the continuum corresponds to a value that can be
                characterized mathematically in any useful way. If one considers
                only logical validity, rather than empirical truth, as being
                important for any given mathematical outcome then all of the
                preceding are simply interesting properties of certain abstract
                mathematical entities. When considering <a href="#metamathematics">metamathematical</a> theories of the
                kind used by scientists and engineers, these observations should
                give one pause (to say the least).
            </p>

            <p>
                Quantum Physics, famously, has caused much philosophical angst for
                as long as it has existed. Albert Einstein coined the concept of
                <i>quanta</i> in describing certain properties of light as
                resulting from non-continuous properties of photons. But despite
                having been as responsible as any single individual for having
                invented it, Einstein was very slow to embrace many of the core
                concepts of Quantum Mechanics ("rolling dice with the universe,"
                "spooky action at a distance") even as more and more
                experimental evidence piled up confirming things like
                non-determinism and entanglement.
            </p>

            <p>
                But here's the thing: what appear to be the most paradoxical
                aspects of modern physics are only paradoxical when failing to
                account for some really basic principles of <a href="#metaphysics">metaphysics</a> and metamathematics.
                Nothing
                should be <em>less</em> surprising than the discovery that
                things like billiard balls are not made of particles that behave
                just like tinier billiard balls caroming off one another.
                Generally speaking, individual parts rarely resemble wholes.
                There is nothing clock-like about an individual cog, spring or
                lever even when a collection of such items can be fit together
                to make a clock. To claim that there is anything particle-like
                or wave-like about an electron and then express mystification
                that an electron can somehow be both a particle and a wave is to
                miss entirely that it would be quite mystifying to discover that
                actual particles or waves in matter operating at the scale
                perceptible to human senses are composed of <em>literal</em>
                particles or waves operating at sub-atomic scale. Rather than
                being mystified at the outcome of things like double-slit
                experiments, scientists should have simply observed that, "well,
                of course, things like photons and electrons can't
                <em>really</em> be either particles or waves even though it is
                sometimes useful to treat them analogically like one or the
                other for the purposes of this experiment or that."
            </p>

            <p>
                When trying to understand and explain the nature of reality far
                below the threshold perciptible to human senses, it is all too
                easy to forget that one is <em>always</em> reasoning by
                abstraction and analogy. The mathematical tools available to
                scientists all are based on the notion of a continuum. But the
                whole point of Quantum Physics is that the deepestest, smallest
                levels of reality that we can explore experimentally are
                non-continuous. How could one expect there not to be some
                counter-intuitve results along the way? When viewed from this
                perspective, even some "settled" questions suddenly feel less
                certain. For example, Bell-type experiments have consistently
                shown that hidden variables are nowhere to be found in nature.
                But the reasoning underlying them is built on a dizzying edifice
                of mathematics that is rooted in Calculus. I do not claim to
                know that Bell inequalities are or are not a good model of how
                to test for hidden variables in reality. I simply find the
                argument that they are somehow conclusive not very convincing
                because they ignore the fundamental question of whether or not
                real numbers can, in principle, provide a reliable basis for a
                metamathematical theory of Quantum Physics in the first place.
                Bell-type experiments require supremely exacting setup and
                measurement. Measurement requires assigning numeric values to
                properties of the subjects of the experiments. Exact values can
                only be assigned using computable numbers. Intuitively, it seems
                that there could be enough wiggle room between the cracks left
                when applying continuous mathematical functions to
                non-continuous reality in which to hide some local variables.
                Can anyone ever really know? Maybe not. Everyone should just
                make themselves fine with that. Any other way lies madness.
            </p>

            <p>
                The discipline of Computablity Theory was founded in the 1930's
                by Alonzo Church and Alan Turing. Church took a formal approach.
                He defined a deliberately extremely simple formal language,
                λ-Calculus, for defining <a href="#function">functions</a> in a
                highly abstract fashion. Church's hypothesis was that the result
                of evaluating every possible value of every well-formed formula
                of λ-Calculus was identical to the set of computable numbers.
                Turing used thought-experiments based on idealized devices he
                called <i>a-machines</i> (<i>a</i> for <i>automatic</i> or
                <i>automaton</i>; it was Church who first started calling them
                <i>Turing machines</i>). These were, again, extremely simple
                mechanisms for generating sequences of digits according to
                well-defined rules. The only reason one could not build an
                actual a-machine was that in order satisfy the desired results
                for Computability Theory they would have to be capable of
                producing infinitely long sequences. Turing's hypothesis was
                that the set of all possible outputs of all correctly
                implemented a-machines was identical to the set of computable
                numbers. These two hypotheses have come to be known together as
                the Church-Turing Thesis, and the initial question of
                Computability Theory was considered settled, when it was shown
                that both approaches produce the same results.
            </p>

            <p>
                Church's and Turing's work might have ended up as little more
                than a footnote in the history of mathematics. In fact, their
                work ushered in the Information Age. The way the equivalence of
                their two approaches was proven was to show that the behavior of
                any a-machine could be represented as an expression of
                λ-Calculus and, conversely, that any expression of λ-Calculus
                could be used as a set of instructions for determining the
                behavior of an a-machine. Between them, they invented the notion
                of a programmable digital computer and a programming language as
                a side-effect of purely abstract mathematical inquiries on the
                nature of certain sets of numbers. (Anonymous function in most
                programming languages are referred to as "lambdas" due to
                λ-Calculus. The classical Von Neumann Architecture for digital
                computers was directly inspired by the idea of embodying a
                restricted, i.e. finite, version of Turing's a-machines using
                electronic circuits rather than mechanical components and paper
                tapes, as in Turing's writings on Computability Theory.)
            </p>

            <h2>A Not-So-Brief History of How This Happened</h2>

            <p>
                From time immemorial, mathematicians in the Islamic and Western
                European tradition were wary of Euclid's fifth "postulate" in
                his axiomatic treatment of planar Geometry:
            </p>

            <blockquote>
                <p>
                    That, if a straight line falling on two straight lines make
                    the interior angles on the same side less than two right
                    angles, the two straight lines, if produced indefinitely,
                    meet on that side on which are the angles less than the two
                    right angles.
                </p>
                <p>
                    - Euclid, <i>Elements</i>, translated by Heath [1908]
                </p>

            </blockquote>

            <aside>
                <blockquote>
                    <p>
                        In a plane, given a line and a point not on it, at most
                        one line parallel to the given line can be drawn through
                        the point.
                    </p>
                    <p>
                        - Playfair [1846]
                    </p>
                </blockquote>
            </aside>

            <p>
                Everyone recognized that the "parallel postulate," as it came to
                be known despite the word "parallel" not appearing anywhere in
                it, was necessary &mdash; or something equivalent, like
                Playfair's Axiom (which <em>does</em> mention parallel lines)
                &mdash; in order to make Euclid's system work. The trouble is
                that the first four of Euclid's axioms are extremely short,
                declarative statements that need no explanation or justification
                to be understood and accepted intuitively. Expressed in
                contemporary English:
            </p>

            <div>
                <ol>

                    <li>
                        Any two points can be joined by a straight line.
                    </li>

                    <li>
                        Any line can be extended indefinitely from either end.
                    </li>

                    <li>
                        A line can be rotated around one end to form a circle.
                    </li>

                    <li>
                        All right (i.e. 90°) angles are equal to each other.
                    </li>

                </ol>
            </div>

            <p>
                By comparison, the fifth axiom is long and complex, and most
                people benefit from a diagram in order to understand what it is
                even saying:
            </p>

            <ParallelPostulate />

        </div>

        <div id="definitions">

            <p class="title">
                Definitions
            </p>

            <details name="definitions">
                <summary><dfn>Bijective Mapping</dfn></summary>
                <a id="bijective"></a>
                <p>
                    A bijective mapping is one that is both <a href="#injective">injective</a> and <a
                        href="#surjective">surjective</a>. Since this establishes a
                    1:1 relationship between every element of the domain and a
                    unique corresponding element of the co-domain, the <a href="#cardinality">cardinalities</a> of both
                    sets are
                    considered equal.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Cardinality</dfn></summary>
                <a id="cardinality"></a>
                <p>
                    The cardinality of a finite set is the number of
                    elements it contains. E.g. the cardinality of
                    <math>
                        <mo>{</mo>
                        <mi>a</mi>
                        <mo>,</mo>
                        <mi>b</mi>
                        <mo>,</mo>
                        <mi>c</mi>
                        <mo>}</mo>
                    </math>
                    is 3.
                </p>
                <p>
                    For infinite sets, cardinality is defined by constructing
                    mappings, i.e. <a href="#function">functions</a>. For
                    example, if a <a href="#bijective">bijective mapping</a> can
                    be shown to exist between two infinite sets that shows they
                    have the same cardinality. Cardinalities can be ordered by
                    showing that mappings between various sets can be at best <a href="#injective">injective</a> or <a
                        href="#surjective">surjective</a>. This, in turn, is used to
                    define a sequence of infinite <a href="#ordinal"> ordinal
                        numbers</a> analogous to the ordering defined by the
                    sequence of natural numbers.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Completed Infinity</dfn></summary>
                <a id="infinity"></a>
                <p>
                    The term <i>completed infinity</i> may refer to a set whose
                    <a href="#cardinality">cardinality</a> is infinite, or to
                    such a set's cardinality when used in a numerical context.
                    For most of the history of Mathematics in the Islamic and
                    Western European tradition, the very notion of a completed
                    infinity was anathema. One was not allowed to talk about
                    things like "the set of natural numbers." Even where
                    infinite or infinitessimal quantities were found to be
                    useful, such as <i>limits</i> in Calculus or proofs by
                    mathematical induction, mathematicians took pains to
                    describe them in terms of non-terminating processes rather
                    than as definite numbers or actual collections.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Continuum</dfn></summary>
                <a id="continuum"></a>
                <p>
                    The <i>continuum</i> is what you might have been taught to
                    refer to as "the number line" in elementary school. A set of
                    numbers is continuous if there are an infinite number of
                    values between any two members of the set. The set of
                    natural numbers, ℕ, is not continuous because there is no
                    natural number whose value is greater than 1 and less than
                    2, nor a number greater than 2 and less than 3, and so on.
                    The real numbers, ℝ, are assumed to be continuous by
                    definition. One consequence of this assumption is that any
                    possible sequence of digits conforming to the rules of
                    mathematical notation denotes a particular real quantity,
                    which may or may not correspond to any natural or rational
                    number, or even a computable number. In fact, the nature of
                    the continuum implies that only an infinitessimal percentage
                    of real numbers are computable. This begs two questions:
                </p>

                <ol>
                    <li>What distinguishes the subset of computable numbers></li>
                    <li>How "real" are non-computable numbers></li>
                </ol>

                <p>
                    The first of the preceding questions launched the field of
                    Computability Theory. The second is asked less often, but is
                    of increasing significance for <a href="#metamathematics">metamathematical</a> theories on
                    which scientists and engineers rely as technologies advance
                    that analyze and utilize quantum effects.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Countable</dfn></summary>
                <a id="countable"></a>
                <p>
                    Any set, <i>S</i>, is countable if it is possible to
                    define an <a href="#injective">injective mapping</a>
                    from the set of natural numbers, <i>ℕ</i> onto
                    <i>S</i>. Every finite set is countable. An infinite set is
                    countable if it is possible to construct a <a href="#bijective">bijective mapping</a> between it and
                    ℕ.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Function</dfn></summary>
                <a id="function"></a>
                <p>
                    In Set Theory, a function is a set of pairs where the
                    set consisting of the first member of each pair is
                    called the function's domain and the set consisting of
                    the second member of each pair is called its co-domain
                    or range. Any set of pairs defines a function. Functions
                    with distinctive constraints such as <a href="#injective">injective</a>, <a
                        href="#surjective">surjective</a> and <a href="#bijective">bijective</a> mappings are
                    frequently
                    used to prove relationships between various infinite
                    sets.
                </p>
                <p>
                    In Church's λ-Calculus, functions are the values denoted by
                    λ expressions. For example,
                    <math>
                        <mo>λ</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mrow>
                            <mi>x</mi>
                            <mi>x</mi>
                        </mrow>
                    </math>
                    defines a function that applies whatever function is
                    passed as a parameter to itself. The term,
                    <math>
                        <mrow v-for="count in 2">
                            <mo>(</mo>
                            <mo>λ</mo>
                            <mi>x</mi>
                            <mo>.</mo>
                            <mrow>
                                <mi>x</mi>
                                <mi>x</mi>
                            </mrow>
                            <mo>)</mo>
                        </mrow>
                    </math>
                    applies that function to itself.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Injective Mapping</dfn></summary>
                <a id="injective"></a>
                <p>
                    An injective mapping is a <a href="#function">function</a>
                    that assigns a unique element from its domain to every
                    element of its range. For any two sets, <i>A</i> and
                    <i>B</i>, if an injective mapping from <i>A</i> to <i>B</i>
                    exists, but not a <a href="#surjective">surjective
                        mapping</a> then <i>A</i> has a <a href="#cardinality">cardinality</a> less than or equal to
                    that of <i>B</i>.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Metamathematics</dfn></summary>
                <a id="metamathematics"></a>
                <p>
                    Once mathematicians replaced empirical verifiabily with
                    logical validity as the ultimate goal of mathematical
                    theories, scientists and engineers came under the obligation
                    of embracing additional theories &mdash; consciously or not
                    &mdash; mapping features of a given mathematical theory to
                    features of the real world. Such a mapping is referred to as
                    a <i>metamathematical theory</i>. Historically, the
                    metamathematical theory was usually baked into any
                    presentation of a mathematical theory such that the
                    distinction was rarely noticed. The first three of Euclid's
                    axioms, for example, are all equally understandable as
                    axioms and as directions for performing constructions with
                    straight-edge and compass. It was only after mathematics had
                    evolved to the point that theories were developed for things
                    which manifestly can never be constructed in the real world,
                    like infinite sets, that the distinction between mathematics
                    and metamathematics became a topic of serious, explicit
                    philosophical discussion.
                </p>
                <p>
                    Of course, this revelation did not happen all at once.
                    Leibnitz and Newton, for example, both went to some
                    pains to try to stave off accusations that their
                    versions of Calculus implied the existence of infinite
                    quantities when, of course, they actually do. (Calculus
                    is all about infinitessimal differences in quantity and
                    the simple algebraic inverse of an infinitessimal is an
                    infinite quantity.)
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Metaphysics</dfn></summary>
                <a id="metaphysics"></a>
                <p>
                    The term <i>metaphysics</i> here is being used in the
                    technical sense of that branch of Analytic Philosophy
                    concerned with the ultimate nature of reality. It should not
                    be confused with popular usage having to do with mystical or
                    spiritual beliefs. Traditional conundrums of Metaphysics as
                    an academic discipline arise from the closely related field
                    of Epistemology, the study of what it is possible to know.
                    The historical dialectic between Realism and Idealism, for
                    example, involving the sounds falling tress make in
                    uninhabited forests lives on in fringe scientific theories
                    that claim Quantum Physics implies that we are living in a
                    multiverse (vs the Copenhagen interpretation, which simply
                    begs for a move like Berkeley's attempt to rescue his
                    version of Idealism by claiming that God functions as a
                    universal observer by which objective reality is maintained)
                    or that we are all just NPC's in some super-intelligence's
                    virtual reality game. Is it not more plausible to think that
                    there is just something missing from our understanding of
                    concepts like superposition and quantum indeterminacy than
                    that existence depends on an observer? If the latter were
                    true, it is hard to see how an appeal to religion can be
                    avoided in order to reconcile the possibility of the Big
                    Bang occuring before any physical observer could, in
                    principle, exist. (This is a perfect example of why
                    discussions of academic Metaphysics are so easily conflated
                    with "metaphysics" of the sage-burning, crystal-brandishing
                    persuasion.)
                </p>
            </details>

            <details name="definitions">
                <summary><a kd="number"></a><dfn>Number</dfn></summary>
                <p>
                    Traditionally, mathematicians have discussed various
                    named sets of numbers, grouped according to how their
                    elements are constructed.
                </p>
                <p>
                    The set of <i>natural numbers</i>, ℕ, is defined
                    inductively. (Historically, ℕ was usually defined
                    so as to exclude 0 while in modern times it is conventional
                    to start the indcution with 0. From a Philosophical point of
                    view, it makes no substantive difference.)
                </p>
                <math display="block">
                    <mi>ℕ</mi>
                    <mspace width="0.2em" />
                    <mo>=</mo>
                    <mspace width="0.2em" />
                    <mrow>
                        <mo>{</mo>
                        <mi>n</mi>
                        <mo>:</mo>
                        <mspace width="0.2em" />
                        <mrow>
                            <mrow>
                                <mi>n</mi>
                                <mo>=</mo>
                                <mn>0</mn>
                            </mrow>
                            <mspace width="0.2em" />
                            <mo>∨</mo>
                            <mspace width="0.2em" />
                            <mrow>
                                <mo>∃</mo>
                                <mi>m</mi>
                                <mrow>
                                    <mo>(</mo>
                                    <mrow>
                                        <mi>m</mi>
                                        <mo>∈</mo>
                                        <mi>ℕ</mi>
                                    </mrow>
                                    <mspace width="0.2em" />
                                    <mo>∧</mo>
                                    <mspace width="0.2em" />
                                    <mrow>
                                        <mi>n</mi>
                                        <mo>=</mo>
                                        <mrow>
                                            <mi>m</mi>
                                            <mo>+</mo>
                                            <mn>1</mn>
                                        </mrow>
                                    </mrow>
                                    <mo>)</mo>
                                </mrow>
                            </mrow>
                        </mrow>
                        <mo>}</mo>
                    </mrow>
                </math>
                <p>
                    Informally, ℕ is the set consiting of 0 and
                    every number that can be obtained by adding one to some
                    existing element of ℕ.
                </p>
                <p>
                    To continue informally, the set of integers, ℤ,
                    is the union of ℕ and the set that results from
                    negating every element of ℕ. (Historically,
                    many arguments that refer to ℕ actually apply
                    to ℤ.)
                </p>
                <p>
                    The set of rational numbers, ℚ, contains all
                    possible pairs of elements from ℤ with the
                    exception of pairs where the second (denominator) member
                    is 0.
                </p>

                <p>
                    Note that if one accepts
                    <math>
                        <mn>∞</mn>
                    </math> as a legitimate member of
                    ℚ then the only pair that needs exclusion is
                    <math>
                        <mo>&lt;</mo>
                        <mn>0</mn>
                        <mo>,</mo>
                        <mn>0</mn>
                        <mo>&gt;</mo>
                    </math> since
                </p>
                <math display="block">
                    <mrow>
                        <mrow>
                            <mrow>
                                <munder>
                                    <mo>lim</mo>
                                    <mrow>
                                        <mi>x</mi>
                                        <mo>→</mo>
                                        <mn>∞</mn>
                                    </mrow>
                                </munder>
                                <mo>(</mo>
                                <mfrac>
                                    <mn>1</mn>
                                    <mn>x</mn>
                                </mfrac>
                                <mo>)</mo>
                            </mrow>
                        </mrow>
                        <mo>=</mo>
                        <mn>0</mn>
                    </mrow>
                    <mspace width="0.2em" />
                    <mo>≡</mo>
                    <mspace width="0.2em" />
                    <mrow>
                        <mrow>
                            <mrow>
                                <munder>
                                    <mo>lim</mo>
                                    <mrow>
                                        <mi>x</mi>
                                        <mo>→</mo>
                                        <mn>0</mn>
                                    </mrow>
                                </munder>
                                <mo>(</mo>
                                <mfrac>
                                    <mn>1</mn>
                                    <mn>x</mn>
                                </mfrac>
                                <mo>)</mo>
                            </mrow>
                        </mrow>
                        <mo>=</mo>
                        <mn>∞</mn>
                    </mrow>
                </math>
                <p>
                    The set of real numbers, ℝ, is harder to define
                    because it represents a mathematical abstraction with
                    little or no <a href="#metamathematics">metamathematical</a> reality.
                    Historically, it is understood as the set of points in
                    the <a href="#continuum">continuum</a>, which is hardly
                    helpful. It can also be visualized as the set of all
                    possible sequences of digits that conform to
                    conventional mathematical notation for real numbers,
                    which verges on but just barely manages to avoid
                    circularity.
                </p>
                <p>
                    While incredibly useful for mathematical theories like
                    Calculus, with all their <a href="#metamathematics">metamathematical</a> utility in
                    science and engineering, the notion of <a href="#continuum">continuum</a>, itself, is incredibly
                    problematic and directly gives rise to the issues
                    addressed by Computability Theory.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Ordinal</dfn></summary>
                <a id="ordinal"></a>
                <p>
                    Since <a href="#cardinality">cardinality</a> provides a way
                    of assigning magnitudes to infinite sets analogous to the
                    number of elements contained in finite sets, such magnitudes
                    can be treated like an ordered set of infinite quantities
                    analagous to the ordered set of natural numbers. The first
                    such infinite ordinal corresponds to the cardinality of ℕ
                    and is conventionally notated as
                    <math>
                        <msub>
                            <mi>ℵ</mi>
                            <mn>0</mn>
                        </msub>
                    </math>. According to the <a href="#continuum">continuum
                        hypothesis</a>, the next infinite ordinal,
                    <math>
                        <msub>
                            <mi>ℵ</mi>
                            <mn>1</mn>
                        </msub>
                    </math>, is the cardinality of ℝ.
                </p>
            </details>

            <details name="definitions">
                <summary><dfn>Surjective Mapping</dfn></summary>
                <a id="surjective"></a>
                <p>
                    A surjective mapping is a <a href="#function">function</a> that assigns every element
                    of its domain to a unqiue element of its range.
                </p>
            </details>

        </div>

    </div>

</template>

<style scoped>
#computability,
#definitions {
    position: absolute;
    overflow: auto;
    padding: 0 1em;
}

#computability {
    left: 0;
    top: 0;
    width: 60%;
    height: 100%;
}

#definitions {
    right: 0;
    top: 0;
    width: 33%;
    height: 100%;
}
</style>

<script setup>
import ParallelPostulate from '@/pages/logic/computability/ParallelPostulate.vue'
</script>