<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <h1>&lambda;-Calculus</h1>

        <p>
            Church defined &lambda;-Calculus for the purpose of constructing the
            set of computable numbers entirely using a highly abstract model of
            computation. To achieve his goal, he gave &lambda;-Calculus a
            deliberately impoverished syntax and semantics:
        </p>

        <ul>
            <li>
                There is only one primitive data type, <i>unary function</i>.
            </li>
            <li>
                There are only two operators, <i>function application</i> and
                <i>&lambda; abstraction</i>.
            </li>
            <li>
                &lambda; abstraction is a variable-binding operator, which is
                the only means by which functions are given names.
            </li>
        </ul>

        <h2>Grammar</h2>

        <p>
            The "strict" grammar for &lambda;-Calculus can be expressed with just a
            handful of BNF productions:
        </p>

        <pre>
            term := name | application | abstraction
            name := a | ... | z
            application := (term term)
            abstraction := (&lambda; name . term)
        </pre>

        <p>
            Use of a name is <i>bound</i> when it occurs within the scope of a
            &lambda; abstraction (i.e. inside the parentheses, following the
            dot) with that name appearing between the &lambda; operator and the
            dot. A name is <i>free</i> otherwise. A term is <i>closed</i> if it
            contains no free names. A term is <i>open</i> otherwise. Only closed
            terms can be used in computation since there is no way to determine
            the value referenced by a free name. Note that where &lambda;
            abstractions are involved, a term may be locally free in an inner
            scope but bound by some outer abstraction within which the given one
            occurs.
        </p>

        <p>
            For example, <math>
                <mi>f</mi>
            </math> is free in
            <math>
                <mo>(</mo>
                <mo>&lambda;</mo>
                <mo>x</mo>
                <mo>.</mo>
                <mrow>
                    <mo>(</mo>
                    <mi>f</mi>
                    <mspace width="0.5em" />
                    <mi>x</mi>
                    <mo>)</mo>
                </mrow>
                <mo>)</mo>
            </math>
            but is bound in
            <math>
                <mo>(</mo>
                <mo>&lambda;</mo>
                <mi>f</mi>
                <mi>.</mi>
                <mo>(</mo>
                <mo>&lambda;</mo>
                <mo>x</mo>
                <mo>.</mo>
                <mrow>
                    <mo>(</mo>
                    <mi>f</mi>
                    <mspace width="0.5em" />
                    <mi>x</mi>
                    <mo>)</mo>
                </mrow>
                <mo>)</mo>
                <mo>)</mo>
            </math>. This must be taken into account when performing operations
            like &alpha; or &eta; conversions.
        </p>

        <details>

            <summary>Conversions and Reductions</summary>

            <p>
                In order to prove certain theorems in Computability Theory as
                well as to use &lambda;-Calculus as a model for programming
                languages it is often necessary to perform certain
                transformations on terms known as <i>conversions</i> and
                <i>reductions</i>.
            </p>

            <dl>
                <dt><i>&alpha; conversion</i></dt>
                <dd>
                    Replace the name of a bound variable. E.g.
                    <math>
                        <mrow>
                            <mo>(</mo>
                            <mo>&lambda;</mo>
                            <mi>x</mi>
                            <mo>.</mo>
                            <mi>x</mi>
                            <mo>)</mo>
                        </mrow>
                        <mo>&equiv;</mo>
                        <mrow>
                            <mo>(</mo>
                            <mo>&lambda;</mo>
                            <mi>y</mi>
                            <mo>.</mo>
                            <mi>y</mi>
                            <mo>)</mo>
                        </mrow>
                    </math>
                </dd>
                <dt><i>&beta; reduction</i></dt>
                <dd>
                    Replace an application with its result. E.g.
                    <math>
                        <mrow>
                            <mo>(</mo>
                            <mrow>
                                <mo>(</mo>
                                <mo>&lambda;</mo>
                                <mi>x</mi>
                                <mo>.</mo>
                                <mi>x</mi>
                                <mo>)</mo>
                            </mrow>
                            <mi>a</mi>
                            <mo>)</mo>
                        </mrow>
                        <mo>&equiv;</mo>
                        <mi>a</mi>
                    </math>
                </dd>
                <dt><i>&eta; conversion</i></dt>
                <dd>
                    Remove (&eta; reduction) or insert (&eta; expansion) vacuous
                    abstractions. An abstraction that does not refer to its
                    parameter is vacuous in the sense that the parameter does
                    not actually affect the result of applying the abstraction
                    to any particular parameter. E.g.
                    <math>
                        <mrow>
                            <mo>(</mo>
                            <mrow>
                                <mo>(</mo>
                                <mo>&lambda;</mo>
                                <mi>x</mi>
                                <mo>.</mo>
                                <mrow>
                                    <mo>(</mo>
                                    <mrow>
                                        <mo>(</mo>
                                        <mo>&lambda;</mo>
                                        <mi>x</mi>
                                        <mo>.</mo>
                                        <mi>x</mi>
                                        <mo>)</mo>
                                    </mrow>
                                    <mi>b</mi>
                                    <mo>)</mo>
                                </mrow>
                                <mo>)</mo>
                            </mrow>
                            <mi>a</mi>
                            <mo>)</mo>
                        </mrow>
                        <mo>&equiv;</mo>
                        <mrow>
                            <mo>(</mo>
                            <mrow>
                                <mo>(</mo>
                                <mo>&lambda;</mo>
                                <mi>x</mi>
                                <mo>.</mo>
                                <mi>x</mi>
                                <mo>)</mo>
                            </mrow>
                            <mi>b</mi>
                            <mo>)</mo>
                        </mrow>
                    </math>
                    Generally,
                    <math>
                        <mo>(</mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mo>(</mo>
                        <mi>f</mi>
                        <mspace width="0.5em" />
                        <mi>x</mi>
                        <mo>)</mo>
                        <mo>)</mo>
                    </math> is interchangeable with
                    <math>
                        <mi>f</mi>
                    </math> so long as
                    <math>
                        <mi>x</mi>
                    </math> does not appear free anywhere within
                    <math>
                        <mi>f</mi>
                    </math>.
                </dd>
            </dl>

            <p>
                Care must be taken to preserve the meaning of &lambda; terms
                when converting and reducing. For example, you can alter the
                meaning of a term by a careless conversion or reduction that
                results in "variable capture," where substituting a name with
                another that already appears locally free will cause what should
                be names of two distinct parameters to coalesce into a single
                value. What this means in practice is that &alpha; conversions
                must often be performed stratigically within a sequence of
                &beta; reductions and &eta; conversions to achieve the correct
                results.
            </p>

        </details>

        <h3>Informal Syntax</h3>

        <p>
            In practice (beginning with Church, himself) the strict grammar is
            rarely used. For example, parentheses can often be omitted without
            introducing ambiguity. Various conventions such as left-associativity of
            application:
        </p>

        <math display="block">
            <mtable>
                <mtr>
                    <mtd>
                        <mi>T</mi>
                        <mi>U</mi>
                        <mi>V</mi>
                    </mtd>
                    <mtd>
                        <mo>&equiv;</mo>
                    </mtd>
                    <mtd>
                        <mo>(</mo>
                        <mo>(</mo>
                        <mi>T</mi>
                        <mspace width="0.5em" />
                        <mi>U</mi>
                        <mo>)</mo>
                        <mi>V</mi>
                        <mo>)</mo>
                    </mtd>
                    <mtd>&nequiv;</mtd>
                    <mtd>
                        <mo>(</mo>
                        <mi>T</mi>
                        <mo>(</mo>
                        <mo>U</mo>
                        <mo>V</mo>
                        <mo>)</mo>
                        <mo>)</mo>
                    </mtd>
                </mtr>
            </mtable>
        </math>

        <p>
            and precedence of application over abstraction:
        </p>

        <math display="block">
            <mtable>
                <mtr>
                    <mtd>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>T</mi>
                        <mi>U</mi>
                        <mi>V</mi>
                    </mtd>
                    <mtd>
                        <mo>&equiv;</mo>
                    </mtd>
                    <mtd>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mo>(</mo>
                        <mo>(</mo>
                        <mi>T</mi>
                        <mspace width="0.5em" />
                        <mi>U</mi>
                        <mo>)</mo>
                        <mi>V</mi>
                        <mo>)</mo>
                    </mtd>
                    <mtd>
                        <mo>&nequiv;</mo>
                    </mtd>
                    <mtd>
                        <mo>(</mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mo>(</mo>
                        <mi>T</mi>
                        <mspace width="0.5em" />
                        <mi>U</mi>
                        <mo>)</mo>
                        <mo>)</mo>
                        <mi>V</mi>
                    </mtd>
                </mtr>
            </mtable>
        </math>

        <p>
            allow for even fewer parentheses. In addition, sequences of
            abstractions are often abbreviated into a single &lambda;
            abstraction with multiple variables:
        </p>

        <math display="block">
            <mtable>
                <mtr>
                    <mtd>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mi>y</mi>
                        <mo>.</mo>
                        <mi>x</mi>
                        <mspace width="0.5em" />
                        <mi>y</mi>
                    </mtd>
                    <mtd>
                        <mo>&equiv;</mo>
                    </mtd>
                    <mtd>
                        <mo></mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mrow>
                            <mo>&lambda;</mo>
                            <mi>y</mi>
                            <mo>.</mo>
                            <mrow>
                                <mi>x</mi>
                                <mspace width="0.5em" />
                                <mi>y</mi>
                            </mrow>
                        </mrow>
                    </mtd>
                </mtr>
            </mtable>
        </math>

        <p>
            These more relaxed conventions are used througout these pages, along
            with even more relaxed implicit conversion of Church Numerals (see
            below) and mathematical operations on them into conventional
            arithmetical notation in the bodies of &lambda; abstractions.
        </p>

        <p class="indent">
            [As the author of this document can attest from personal experience
            while attending seminars on "Topics in Metamathematics" given by
            Church at UCLA in the 1970's, he would invariably use prefix (a.k.a.
            "Polish") notation for mathematical operations when writing informal
            &lambda; expressions on chalkboards and would never miss an
            opportunity to discuss its advantages, primarily for the
            opportunities it affords for eliminating even more parentheses.
            Church's aversion to parentheses and the efforts he took to
            minimizze any need for them in &lambda; notation makes the
            over-abundance of parentheses in the Lisp family of programming
            languages more than a little ironic. Historically, the reason for
            Lisp's idiosyncratic syntax is that <i>s-exprs</i> (structured
            expressions) were originally intended as a representation of
            &lambda; terms used internally by Lisp interpreters and compilers.
            Lisp's creators intended eventually to define a separate
            representation, <i>m-exprs</i>, with a syntax modeled on Algol (the
            progenitor of all conventional programming language syntax) for use
            when writing actual programs in Lisp. Fortunately or unfortunately,
            Lisp escaped into the wild before M syntax had developed far enough
            to be useful. Programmers in the real world became sufficiently used
            to S syntax that any motivation for the development of M syntax
            simply evaporated in the mainstream Lisp development community.
            Instead, a number of alternative languages with identical or nearly
            identical semantics to Lisp were developed over the years, each with
            its own idiosyncratic syntax (Haskell being an early example).
            However, among the reasons that Lisp dialects have remained popular
            with programming language theorists and prototypers to this day is
            that S syntax was designed to be particularly easy to construct for
            output and parse as input. This not only eases the implementation of
            Lisp language processors, but historically allowed for treating a
            given s-expr interchangeably as "code" or "data," in keeping with
            Church's typeless &lambda;-Calculus. A programming language
            interpreter's console interface is often referred to as a REPL
            (Read-Eval-Print Loop) because, once upon a time, a Lisp
            interpreter's outermost process entry point consisted literally of
            <code>(while #t (print (eval (read))))</code>
            or its equivalent in a given Lisp dialect (and note Lisp
            implementers' following Church's lead in embracing prefix
            notation).]
        </p>

        <p>
            Examples:
        </p>

        <dl>
            <dt>
                <math>
                    <mi>a</mi>
                </math>
            </dt>
            <dd>
                The free name <math>
                    <mi>a</mi>
                </math>.
            </dd>
            <dt>
                <math>
                    <mo>&lambda;</mo>
                    <mi>x</mi>
                    <mo>.</mo>
                    <mi>x</mi>
                </math>
            </dt>
            <dd>
                The identity function; i.e. a function that simply returns
                whatever value it is passed as a parameter.
            </dd>
            <dt>
                <math>
                    <mrow>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>x</mi>
                    </mrow>
                    <mo>(</mo>
                    <mi>a</mi>
                    <mo>)</mo>
                </math>
            </dt>
            <dd>
                Application of the identity function to
                <math>
                    <mi>a</mi>
                </math>. Note that parentheses are required, in this case, due
                to the precedence of application over abstraction though it is a
                matter of taste whether the parentheses surround the parameter
                (as shown above) or the function as in
                <math>
                    <mrow>
                        <mo>(</mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>x</mi>
                        <mo>)</mo>
                    </mrow>
                    <mi>a</mi>
                </math>. Church would have preferred the former while hastening
                to point out that trailing parentheses are never actually
                necessary where Polish notation is used and would omit it when
                writing informally on a chalkboard which, in this, results in
                <math>
                    <mrow>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>x</mi>
                    </mrow>
                    <mo>(</mo>
                    <mi>a</mi>
                </math>
            </dd>
            <dt>
                <math>
                    <mrow>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>x</mi>
                    </mrow>
                    <mrow>
                        <mo>(</mo>
                        <mo>&lambda;</mo>
                        <mi>y</mi>
                        <mo>.</mo>
                        <mi>y</mi>
                        <mo>)</mo>
                    </mrow>
                </math>
            </dt>
            <dd>
                Application of the identity function to itself. Note that,
                thanks to &alpha; conversion, the function being applied and the
                function to which it is being applied can be shown to be
                logially identical despite the use of different names for the
                bound variable in each.
            </dd>
        </dl>

        <p>
            Note that the second and fourth of the preceding examples represent
            well-formed &lambda; expressions corresponding to computable values.
            The second expression's value is an anonymous function. Such terms
            are the only way of referring to a specific value in Church's
            typeless &lambda;-Calculus since there no other primitive types and
            certainly nothing analogous to a real-world programming language's
            numeric, boolean or string constants. I.e. in the universe of
            &lambda;-Calculus, the only things that exist are anonymous
            functions. In that regard, the fourth expression's value is actually
            the same as that of the second one, as can be shown by applying
            &beta; reduction and &alpha; conversion:
        </p>

        <table>
            <tbody>
                <tr>
                    <td>1</td>
                    <td>
                        <math>
                            <mrow>
                                <mo>(</mo>
                                <mo>&lambda;</mo>
                                <mi>x</mi>
                                <mo>.</mo>
                                <mi>x</mi>
                                <mo>)</mo>
                            </mrow>
                            <mrow>
                                <mo>(</mo>
                                <mo>&lambda;</mo>
                                <mi>y</mi>
                                <mo>.</mo>
                                <mi>y</mi>
                                <mo>)</mo>
                            </mrow>
                        </math>
                    </td>
                    <td>
                        Application of the identity function to itself
                    </td>
                </tr>
                <tr>
                    <td>2</td>
                    <td>
                        <math>
                            <mo>(</mo>
                            <mo>&lambda;</mo>
                            <mi>y</mi>
                            <mo>.</mo>
                            <mi>y</mi>
                            <mo>)</mo>
                        </math>
                    </td>
                    <td>
                        From 1 by &beta; reduction
                    </td>
                </tr>
                <tr>
                    <td>3</td>
                    <td>
                        <math>
                            <mo>(</mo>
                            <mo>&lambda;</mo>
                            <mi>x</mi>
                            <mo>.</mo>
                            <mi>x</mi>
                            <mo>)</mo>
                        </math>
                    </td>
                    <td>
                        From 2 by &alpha; conversion
                    </td>
                </tr>
            </tbody>
        </table>

        <p>
            This shows that as a model of computation, &beta; reduction is the
            equivalent of evaluating an expression in a programming language.
            Similarly, &alpha; conversion is analogous to "interning" canonical
            values of strings and symbol names as by a programming language's
            run-time library and &eta; reduction is comparable to optimizations
            performed by a compiler when translating from source code to machine
            language. Even &eta; expansion, which appears at first glance to be
            counter-productive code de-optimization, has a useful analog in
            real-world programming contexts in the form of "aspect oriented
            programming" that can inject code to address "cross-cutting
            concerns" like logging at arbitrary points in sequential execution
            (iterative &beta; reduction).
        </p>

        <p>
            Note further that the &beta; reduction and &alpha; conversion could
            have been performed in either order, in this case. Generally
            speaking, it is less error-prone to defer &alpha; conversions until
            the points at which they are strictly necessary to minimize the risk
            of careless capture during intermediate transformations of complex
            expressions.
        </p>

        <p>
            Conversely, though the first and third examples are syntactically
            parseable as &lambda; expressions, they are not semantically
            well-formed due to the presence of the free name
            <math>
                <mi>a</mi>
            </math> in each occurence.
        </p>

        <p>
            The ultimate relaxation of &lambda; grammar is using words like
            <i>let</i> and <i>where</i> to define "global" names for particular
            &lambda; terms that are used repeatedly. This is universally used in
            classrooms and texts because to rely solely on &lambda; abstraction
            to assign names results in expressions that are labor intensive in
            the extreme to read, let alone write correctly. Consider Haskell
            Curry's famous <i>Y combinator</i>:
        </p>

        <YCombinator display="block" />

        <p>
            Curry showed that self-recursion (a function applying itself during
            its own evaluation) is possible using &lambda; terms, which is a
            neat trick given the deliberately limited syntax of &lambda;
            abstractions. But note that the &lambda; term which enables this
            feat is fairly complex with multiple, repetitive, nested &lambda;
            terms:
        </p>

        <math display="block">
            <mrow>
                <mrow>
                    <mo>λ</mo>
                    <mi>g</mi>
                </mrow>
                <mo>.</mo>
                <mrow v-for="count in 2">
                    <mo>(</mo>
                    <mrow>
                        <mo>λ</mo>
                        <mi>x</mi>
                    </mrow>
                    <mo>.</mo>
                    <mrow>
                        <mi>g</mi>
                        <mo>(</mo>
                        <mi>x</mi>
                        <mspace width="0.5em" />
                        <mi>x</mi>
                        <mo>)</mo>
                    </mrow>
                    <mo>)</mo>
                </mrow>
            </mrow>
        </math>

        <p>
            Given that the point of the preceding is to use it in conjuction
            with arbitrarily complex &lambda; abstractions that end up bound to
            its
            <math>
                <mi>g</mi>
            </math> and
            <math>
                <mi>x</mi>
            </math> variables, using it explicitly is nearly impossible to get
            right and excruciating to comprehend. Succinct references using
            abbreviations like
            <math>
                <mi>Y</mi>
            </math> make it extremely clear what is going on and allow authors
            and readers to focus on what is actually important in a given
            formula. (In the case of the Y combinator, what is important is that
            it simultaneously enables the definition of functions that calculate
            things like the Fibonacci series while making it easy to demonstrate
            that Church's simple, typeless &lambda;-Calculus is <router-link
                :to="{ name: 'liar' }">incomplete</router-link>).
        </p>

        <h2>Church Numerals</h2>

        <p>
            In order to accomplish the purpose for which Church created
            &lambda;-Calculus, he needed to create a model of numbers and
            arithmetic operations using them. Since the only native data type in
            Church's original &lambda;-Calculus are funtions, <i>Church
                Numerals</i> model natural numbers by applying a given function the
            corresponding number of times:
        </p>

        <math display="block">
            <mtable>
                <mtr>
                    <mtd>
                        <mtext>Natural Number</mtext>
                    </mtd>
                    <mtd>
                        <mtext>Church Numeral</mtext>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mn>0</mn>
                    </mtd>
                    <mtd>
                        <mo>&lambda;</mo>
                        <mi>f</mi>
                        <mo>.</mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>x</mi>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mn>1</mn>
                    </mtd>
                    <mtd>
                        <mo>&lambda;</mo>
                        <mi>f</mi>
                        <mo>.</mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>f</mi>
                        <mspace width="0.5em" />
                        <mi>x</mi>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mn>2</mn>
                    </mtd>
                    <mtd>
                        <mo>&lambda;</mo>
                        <mi>f</mi>
                        <mo>.</mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>f</mi>
                        <mo>(</mo>
                        <mi>f</mi>
                        <mspace width="0.5em" />
                        <mi>x</mi>
                        <mo>)</mo>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mn>3</mn>
                    </mtd>
                    <mtd>
                        <mo>&lambda;</mo>
                        <mi>f</mi>
                        <mo>.</mo>
                        <mo>&lambda;</mo>
                        <mi>x</mi>
                        <mo>.</mo>
                        <mi>f</mi>
                        <mo>(</mo>
                        <mi>f</mi>
                        <mo>(</mo>
                        <mi>f</mi>
                        <mspace width="0.5em" />
                        <mi>x</mi>
                        <mo>)</mo>
                        <mo>)</mo>
                    </mtd>
                </mtr>
                <mtr>
                    <mtd>
                        <mtext>&vellip;</mtext>
                    </mtd>
                    <mtd>
                        <mtext>&vellip;</mtext>
                    </mtd>
                </mtr>
            </mtable>
        </math>

        <p>
            Generally, the Chuch numeral corresponding to any natural number,
            <math>
                <mi>n</mi>
            </math>, is
            <math>
                <mi>n</mi>
                <mo>=</mo>
                <mrow>
                    <mo>&lambda;</mo>
                    <mi>f</mi>
                    <mo>.</mo>
                    <mo>&lambda;</mo>
                    <mi>x</mi>
                    <mo>.</mo>
                    <msup>
                        <mi>f</mi>
                        <mrow>
                            <mspace width="0.25em" />
                            <mi>n</mi>
                        </mrow>
                    </msup>
                    <mo>(</mo>
                    <mi>x</mi>
                    <mo>)</mo>
                </mrow>
            </math>
            where
            <math>
                <msup>
                    <mi>f</mi>
                    <mrow>
                        <mspace width="0.25em" />
                        <mi>n</mi>
                    </mrow>
                </msup>
                <mo>(</mo>
                <mi>x</mi>
                <mo>)</mo>
            </math>
            is shorthand for applying
            <math>
                <mi>f</mi>
            </math>
            <math>
                <mi>n</mi>
            </math>
            times in succession, initially passing it
            <math>
                <mi>x</mi>
            </math>. It is important to undersand that the Church number 0 isn't
            the number zero, it is a function that returns the identity
            function. The Church number 1 isn't the number one, it is a function
            that takes a function that takes a function as an argument and that
            applies its argument one time to whatever is passed as an argument
            to <em>that</em> function. And so on. It is simply a convention to
            equate a function that applies a given one
            <math>
                <mi>n</mi>
            </math>
            times to the number
            <math>
                <mi>n</mi>
            </math>.
        </p>

        <p>
            Having defined a convention for modeling numbers as functions,
            Church then defined a convention for modeling arithmetic operations
            by composing such functions. In the end, he provided a model of
            numerical calculation sufficient to prove that the value for any
            formula of arithmetic could be encoded as a &lambda; expression. The
            same can be done for Boolean values, the logical connectives of the
            Sentential Calculus, and so on. Church achieved his goal of defining
            a general-purpose abstraction of the concept of
            <router-link :to="{ name: 'computability' }">
                "computation."
            </router-link>
        </p>

    </div>

</template>

<style scoped>
.indent {
    margin-left: 2rem;
    font-style: italic;
}

.indent i,
.indent em,
.indent pre,
.indent code {
    font-style: normal;
}
</style>

<script setup lang="ts">
import YCombinator from '../liar/YCombinator.vue';
</script>