<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <MathView>
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
    </MathView>
</template>

<script setup>
import MathView from '@/components/MathView.vue'
</script>