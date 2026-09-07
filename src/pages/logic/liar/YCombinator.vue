<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <math :display="display">
        <component :is="display === 'block' ? 'mtable' : 'mrow'">
            <component :is="display === 'block' ? 'mtr' : 'mrow'">
                <component :is="display === 'block' ? 'mtd' : 'mrow'">
                    <mrow>
                        <mi>Y</mi>
                        <mi>f</mi>
                    </mrow>
                    <mo>≡</mo>
                    <mrow>
                        <mi>f</mi>
                        <mo>(</mo>
                        <mi>Y</mi>
                        <mi>f</mi>
                        <mo>)</mo>
                    </mrow>
                </component>
            </component>
            <component :is="display === 'block' ? 'mtr' : 'mrow'">
                <component :is="display === 'block' ? 'mtd' : 'mrow'">
                    <mtext>where</mtext>
                    <mspace width="0.5em" />
                    <mrow>
                        <mi>Y</mi>
                        <mo>=</mo>
                        <mrow>
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
                        </mrow>
                    </mrow>
                </component>
            </component>
        </component>
    </math>
</template>

<script setup>
const {display}=defineProps({

    display: {
        type: String,
        default: 'inline',
    },
})
</script>