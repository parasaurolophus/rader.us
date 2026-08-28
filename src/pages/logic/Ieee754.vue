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

        <mspace width="1em" />
        <mo>&equiv;</mo>
        <mspace width="1em" />

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
        since the latter is the algebraic inverse of the former.
    </p>

    <p>
        However:
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
        <mo>&equiv;</mo>
        <mn>NaN</mn>
    </math>

    <p>
        due to conflicting axioms.
    </p>

    <ul>
        <li>
            On the one hand, dividing any number by 0 ought to produce an
            infinite quotient, as shown above.
        </li>
        <li>
            On the other hand, any number divided by itself ought to produce a
            quotient of 1 according to the axioms of arithmetic.
        </li>
    </ul>

    <p>
        Since <math>
            <mfrac>
                <mn>0</mn>
                <mn>0</mn>
            </mfrac>
        </math> cannot be both 1 and &infin;, it must be neither. Since there
        are no other possibilities, the actual result is <code>NaN</code> ("not
        a number").
    </p>

    <p>
        The IEEE specification allows implementations to generate an OS trap or
        throw an excaption instead of returning <code>NaN</code>. The same
        applies in other cases where <code>NaN</code> would be returned, such as:
    </p>

    <math display="block">
        <mtable>
            <mtr>
                <mtd>
                    <msqrt>
                        <mo>&minus;</mo>
                        <mi>x</mi>
                    </msqrt>
                    <mo>&equiv;</mo>
                    <mn>NaN</mn>
                </mtd>
            </mtr>
            <mtr>
                <mtd>
                    <mtext>where</mtext>
                    <mspace width="1em"/>
                    <mrow>
                        <mi>x</mi>
                        <mo>&gt;</mo>
                        <mn>0</mn>
                    </mrow>
                </mtd>
            </mtr>
        </mtable>
    </math>

</template>