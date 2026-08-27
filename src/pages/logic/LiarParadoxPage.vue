<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <h1>Liar Paradox</h1>

    <p>
        A number of seemingly unrelated results in formal linguistics,
        combinatory logic and computer science can be understood as all being
        variants of the <i>liar paradox</i>. As has been discussed in first-year
        Philosphy classes from at least the time of the ancient Greeks, certain
        simple, declarative sentences cannot be said to be either true or false.
        If I say, for example, "I am lying," am I lying or telling the truth? If
        I were lying, what I said would be true and therefore not a lie which is
        an obvious self-contradiction. So the sentence, "I am lying" can be
        neither true nor false. In the terminology of formal linguistics and
        truth-functional semantics it cannot be assigned a truth-value. (Which
        is as simple a refutation of the viability of truth-functional semantics
        as anyone should need, but I digress....)
    </p>

    <p>
        The crux of the issue demonstrated by this simple, classic expression of
        the liar paradox arises from self-referencing expressions. "Fred is
        lying" can be shown empirically to be either true or false by comparing
        Fred's claims to observable reality. It is self-reference in "I am
        lying" that triggers the paradox. But there is more to this paradox than
        simple self-reference. Who among us has never in their lives told even a
        little fib? Thus "I lied" is not only verifiably true or false, it is
        almost certainly true of everyone who has ever learned to speak. The
        paradox arises not from speakers refering to themselves per se, but is
        intrinsic to certain utterances where an utterance, itself, is
        self-referential, with no explicit utterer required. "This sentence is
        false" is another simple example of the liar paradox, without
        attributing the falsehood to any particular speaker. It is a sentence
        referring to its own truth value that gives rise to the paradox.
    </p>

    <aside>
        A language is said to be <dfn id="incomplete">incomplete</dfn>
        if its grammar is such that it can be used to construct well-formed
        expressions which cannot be assigned a value using the rules of the
        language, itself. The liar paradox demonstrates that natural languages
        like ancient Greek and modern English are incomplete. Kurt Goedel proved
        that all formal languages with certain properties (exactly the
        properties that make them useful for general-purpose mathematical,
        scientific and engineering applications) are also incomplete because
        they can be used to form self-referential expressions analogous to "This
        sentence is false."
    </aside>

    <p>
        Formal languages of the kind used by mathematicions and computer
        programmers can also construct self-referencing expressions and so are
        susceptible to the same kind of semantic failure as the liar paradox.
        Consider Alonzo Church's <i>Lambda Calculus</i>. It gave rise to the
        very idea of a programming language before digital computers had been
        invented, in the same way (and as part of the same line of mathematical
        inquiry) as Alan Turing's <i>a-machines</i> gave rise to the idea of
        such digitally programmable devices. (Turing was Church's student and
        subsequent collaborator in the field of <i>Computability Theory</i>.
        Between them, they laid the mathematical foundation on which the
        Information Age was built.) Haskell Curry showed that, despite its
        essential utility not only in helping resolve the original abstract
        questions regarding the nature of computable numbers for which Church
        had created it, the Lambda Calculus is logically <i>incomplete</i>
        precisely because it can be used to construct self-referential
        <i>fixed-point combinators</i> such as <i>Y</i>:
    </p>

    <math display="block">
        <mi>Y</mi>
        <mo>=</mo>
        <mrow>
            <mo>&lambda;</mo>
            <mi>f</mi>
            <mo>.</mo>
            <template v-for="count in 2">
                <mrow>
                    <mo>(</mo>
                    <mo>&lambda;</mo>
                    <mi>x</mi>
                    <mo>.</mo>
                    <mi>f</mi>
                    <mo>(</mo>
                    <mi>x</mi>
                    <mspace width="0.5em" />
                    <mi>x</mi>
                    <mo>)</mo>
                    <mo>)</mo>
                </mrow>
            </template>
        </mrow>
    </math>

    <p>
        A detailed explanation of the meaning and utility of the Y combinator,
        shown above, has been the subject of countless graduate students' theses
        and dissertaions across many university Mathematics, Linguistics,
        Philosphy and Computer Science departments. But suffice it to say here
        that it allows one to define functions within the grammar of the Lambda
        Calculus that call themselves, which is a neat trick given the
        deliberate and ostentatious simplicity of Church's formalism. I.e. it
        can be shown that, given the preceding definition of Y, it is possible
        to define functions such that:
    </p>

    <math display="block">
        <mrow>
            <mi>Y</mi>
            <mi>g</mi>
        </mrow>
        <mo>&equiv;</mo>
        <mrow>
            <mi>g</mi>
            <mo>(</mo>
            <mi>Y</mi>
            <mi>g</mi>
            <mo>)</mo>
        </mrow>
    </math>

    <aside>
        Note that Y is not the only fixed-point combinator in the Lambda
        Calculus, but it is the simplest and most famous one. It is possible,
        for example, to create similar combinators that allow for
        mutually-recursive functions (i.e. sets of functions which call each
        other) in addition to individual functions that call themselves.
    </aside>

    <p>
        This allows one to define self-calling functions like <i>!</i> (the
        <i>factorial</i> function) and the formula to produce the Fibonacci
        series. It is also exactly what is necessary to construct well-formed
        lambda expressions which do not produce any results, just as the liar
        paradox arises from grammatically correct utterances in natural language
        that cannot be assigned a truth value through the same kind of
        problematic self-reference. Such self-reference is not confined to the
        abstract universe of formal languages. The original proof that there is
        no general solution to the <i>halting problem</i> relied on pointing out
        the paradoxical behavior of certain kinds of programs when they were
        assigned to analyze their own source code. Even at the applied level of
        actual computer programs written in real-world programming languages,
        self-reference is incredibly useful while leaving code written by unwary
        programmers vulnerable to bugs ranging from stack-overflows to infinite
        loops.
    </p>

    <p>
        None of this should be construed as claiming that all self-referencing
        formulas are invalid. Self- and mutual recursion are essential to any
        proof by mathematical induction. They are also essential to any
        practical programming language. At the level of compiler implementation,
        there is no conceptual difference between looping constructs and
        tail-recursion, so even the most basic structured programming languages
        rely instrinsically on self reference in order to be Turing complete.
        The functional programming paradigm and <abbr>CPS</abbr> (<dfn title="CPS">Continuation Passing Style</dfn>)
        place
        recursion at the
        center of good programming style. Here is a simple example of
        implementing <i>5!</i> in <i>Scheme</i> using recursion:
    </p>

<pre>
(let factorial ((a 1)
    (n 5))
    (if (< n 2) a (factorial (* a n) (- n 1)) ) ) ;=> 120
</pre>

    <p>
        Since <code>let</code> is syntactic sugar for <code>lambda</code>, the
        preceding definition of <i>5!</i> could be replaced by an invocation of
        a Scheme implementation of <i>Y</i>. Such a definition would be far
        longer than the idiomatic Scheme expression, much more difficult to
        understand, and substantially less efficient at run time. But both
        versions would return the number 120 as their result when executed.
    </p>

</template>
