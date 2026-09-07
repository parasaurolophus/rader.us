<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <h1>The Liar Paradox</h1>

        <p>
            A number of seemingly unrelated results in formal linguistics,
            combinatory logic and computer science can be understood as all
            being variants of the <i>liar paradox</i>. As has been discussed in
            first-year Philosphy classes from at least the time of the ancient
            Greeks, certain simple, declarative sentences cannot be said to be
            either true or false. If I say, for example, "I am lying," am I
            lying or telling the truth? If I were lying, what I said would be
            true and therefore not a lie which is an obvious self-contradiction.
            So the sentence, "I am lying" can be neither true nor false. In the
            terminology of formal linguistics and truth-functional semantics
            it's truth-value is <i>undecidable</i>.
        </p>

        <p>
            The crux of the issue demonstrated by this simple, classic
            expression of the liar paradox arises from self-referencing
            expressions. "Fred is lying" can be shown empirically to be either
            true or false by comparing Fred's claims to observable reality. It
            is self-reference in "I am lying" that triggers the paradox. But
            there is more to this paradox than simple self-reference. Who among
            us has never in their lives told even a little fib? Thus "I lied" is
            not only verifiably true or false, it is almost certainly true of
            everyone who has ever learned to speak. The paradox arises not from
            speakers refering to themselves per se, but is intrinsic to certain
            utterances where an utterance, itself, is self-referential, with no
            explicit utterer required. "This sentence is false" is another
            simple example of the liar paradox, without attributing the
            falsehood to any particular speaker. It is a sentence denying its
            own truth value that gives rise to the paradox.
        </p>

        <h2>Other Paradoxes</h2>

        <p>
            Many historically significant results in various fields of
            mathematics such as Set Theory are similar to the liar paradox in
            that they turn on using a definition which denies itself within the
            context of a given expression. <i>Russell's Paradox</i> is a simple
            example, which showed that Frege's so-called "naive" set-theory had
            a defective axiom:
        </p>

        <RusselParadox display="block" />

        <p>
            I.e. the set of all sets that are not members of themselves cannot
            exist, because if it did it would both have to be a member of itself
            and not a member of itself. (If <i>S</i> were a member of itself,
            that contradicts the property defining <i>S</i> and so it could not
            be in itself. But if <i>S</i> is not a member of itself, then it
            matches the property defining it, meaning it would have to be in
            itself.) So even though the preceding formal definition of
            <i>S</i> is well-formed within the grammar of Set Theory, it is
            undecidable precisely because <i>S</i>'s definition uses <i>s</i>
            in the same pathologically self-referential way as the liar paradox.
            Thus Russell showed that the axiom from Frege's proposed version of
            Set Theory stating that <em>evey well-defined property defines a set
                of elements which have that property</em> leads to
            self-contradiction in the specific case of <i>S</i>.
        </p>

        <h2>Incompleteness</h2>

        <aside>
            A language is said to be <dfn id="incomplete">incomplete</dfn>
            if its grammar is such that it can be used to construct well-formed
            expressions which cannot be assigned a value using the rules of the
            language, itself. The liar paradox demonstrates that natural
            languages like ancient Greek and modern English are incomplete. Kurt
            Gödel proved that all formal languages with certain properties
            (exactly the properties that make them useful for general-purpose
            mathematical, scientific and engineering applications) are also
            incomplete because they can be used to form self-referential
            expressions analogous to "This sentence is false."
        </aside>

        <p>
            Formal languages of the kind used by mathematicions and computer
            programmers can also construct self-referencing expressions and so
            are susceptible to the same kind of semantic failure as the liar
            paradox. Consider Alonzo Church's <i>λ-Calculus</i>. It gave
            rise to the very idea of a programming language before digital
            computers had been invented, in the same way (and as part of the
            same line of mathematical inquiry) as Alan Turing's
            <i>a-machines</i> gave rise to the idea of such digitally
            programmable devices. (Turing was Church's student and subsequent
            collaborator in the field of <i>Computability Theory</i>. Between
            them, they laid the mathematical foundation on which the Information
            Age was built.) Haskell Curry showed that, despite its essential
            utility not only in helping resolve the original abstract questions
            regarding the nature of computable numbers for which Church had
            created it, the λ-Calculus is logically <i>incomplete</i>
            precisely because it can be used to construct self-referential
            <i>fixed-point combinators</i> such as <i>Y</i>:
        </p>

        <YCombinator display="block" />

        <p>
            A detailed explanation of the meaning and utility of the <i>Y</i>
            combinator, shown above, has been the subject of countless graduate
            students' theses and dissertaions across many university
            Mathematics, Linguistics, Philosphy and Computer Science
            departments. But suffice it to say here that it allows one to define
            functions within the grammar of the λ-Calculus that call
            themselves, which is a neat trick given the deliberate and
            ostentatious simplicity of Church's formalism.
        </p>

        <aside>
            Note that <i>Y</i> is not the only fixed-point combinator in the
            λ-Calculus, but it is the simplest and most famous one. It is
            possible, for example, to create similar combinators that allow for
            mutually-recursive functions (i.e. sets of functions which call each
            other) in addition to individual functions that call themselves like
            <i>Y</i>.
        </aside>

        <p>
            This allows one to define self-calling functions like <i>!</i> (the
            <i>factorial</i> function) and the formula to produce the Fibonacci
            series. It is also exactly what is necessary to construct
            well-formed λ-expressions which do not produce any results,
            just as the liar paradox arises from grammatically correct
            utterances in natural language that cannot be assigned a truth value
            through the same kind of problematic self-reference. Such
            self-reference is not confined to the abstract universe of formal
            languages. The original proof that there is no general solution to
            the <i>halting problem</i> relied on pointing out the paradoxical
            behavior of certain kinds of programs when they were assigned to
            analyze their own source code. Even at the applied level of actual
            computer programs written in real-world programming languages,
            self-reference is incredibly useful while leaving code written by
            unwary programmers vulnerable to bugs ranging from stack-overflows
            to infinite loops.
        </p>

        <p>
            In other words, none of this should be construed as claiming that
            all self-referencing formulas are invalid. Self- and mutual
            recursion are essential to any proof by mathematical induction. They
            are also essential to any practical programming language. At the
            level of compiler implementation, there is no conceptual difference
            between looping constructs and tail-recursion, so even the most
            basic structured programming languages rely instrinsically on self
            reference in order to be Turing complete. The functional programming
            paradigm and <abbr>CPS</abbr> (<dfn title="CPS">Continuation Passing
                Style</dfn>) place recursion at the center of good programming
            style. Here is a simple example of implementing <i>5!</i> in
            <i>Scheme</i> using tail recursion:
        </p>

        <pre>
(let factorial ((a 1)
                (n 5))
    (if (< n 2)
        a
        (factorial (* a n)
                   (- n 1)))) ;=> 120
        </pre>

    </div>

</template>

<script setup>
import RusselParadox from '@/pages/logic/liar/RusselParadox.vue'
import YCombinator from '@/pages/logic/liar/YCombinator.vue'
</script>