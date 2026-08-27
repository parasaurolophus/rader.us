<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <h1>IEEE 754 Division by Zero</h1>

    <p>
        Division by zero of most numbers is well defined, and produces a result
        of &pm;&infin; because:
    </p>

    <math display="block">

        <mrow>
            <munder>
                <mo>lim</mo>
                <mrow>
                    <mi>x</mi>
                    <mo>&rarr;</mo>
                    <mrow>
                        <mo>&pm;</mo>
                        <mn>&infin;</mn>
                    </mrow>
                </mrow>
            </munder>
            <mo>(</mo>
            <mfrac>
                <mn>1</mn>
                <mi>x</mi>
            </mfrac>
            <mo>)</mo>
            <mo>=</mo>
            <mrow>
                <mo>&pm;</mo>
                <mn>0</mn>
            </mrow>
        </mrow>

        <mo>&equiv;</mo>

        <mrow>
            <munder>
                <mo>lim</mo>
                <mrow>
                    <mi>x</mi>
                    <mo>&rarr;</mo>
                    <mrow>
                        <mo>&pm;</mo>
                        <mn>0</mn>
                    </mrow>
                </mrow>
            </munder>
            <mo>(</mo>
            <mfrac>
                <mn>1</mn>
                <mi>x</mi>
            </mfrac>
            <mo>)</mo>
            <mo>=</mo>
            <mrow>
                <mo>&pm;</mo>
                <mn>&infin;</mn>
            </mrow>
        </mrow>

    </math>

    <p>
        since the latter is the algebraic inverse of the former. However:
    </p>

    <math display="block">
        <mfrac>
            <mrow>
                <mo>&pm;</mo>
                <mn>0</mn>
            </mrow>
            <mrow>
                <mo>&pm;</mo>
                <mn>0</mn>
            </mrow>
        </mfrac>
        <mo>=</mo>
        <mn>NaN</mn>
    </math>

    <p>
        due to conflicting axioms.
    </p>

    <ul>

        <li>
            On the one hand, any number divided by itself ought to produce a
            quotient of 1.
        </li>

        <li>
            On the other hand, dividing any number by 0 ought to produce an
            infinite quotient, as shown above.
        </li>

    </ul>

    <p>
        Since <math>
            <mfrac>
                <mn>0</mn>
                <mn>0</mn>
            </mfrac>
        </math> cannot be
        both 1 and &infin;, it must be neither. Since there are no other
        possibilities, the actual result is <math>
            <mn>NaN</mn>
        </math> ("not
        a number").
    </p>

</template>