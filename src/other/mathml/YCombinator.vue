<!-- Copyright (c) Kirk Rader 2026 -->

<template>

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

        <mspace width="0.5em" />
        <mtext>where</mtext>
        <mspace width="0.5em" />

        <mi>Y</mi>
        <mo>=</mo>

        <mo>(</mo>
        <mo>&lambda;</mo>
        <mi>f</mi>
        <mo>.</mo>

        <template v-for="count in 2">
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
        </template>

        <mo>)</mo>

    </math>
</template>