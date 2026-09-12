<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <math :display="display">
        <component :is="display === 'block' ? 'mtable' : 'mrow'">
            <component :is="display === 'block' ? 'mtr' : 'mrow'">
                <component :is="display === 'block' ? 'mtd' : 'mrow'">
                    <mi>Ω</mi>
                    <mo>=</mo>
                    <mrow>
                        <mi>ω</mi>
                        <mi>ω</mi>
                    </mrow>
                </component>
            </component>
            <component :is="display === 'block' ? 'mtr' : 'mrow'">
                <component :is="display === 'block' ? 'mtd' : 'mrow'">
                    <template v-if="display === 'inline'">
                        <mtext>,</mtext>
                        <mspace width="0.5rem" />
                    </template>
                    <mtext>where</mtext>
                    <mspace width="0.5rem" />
                    <mrow>
                        <mi>ω</mi>
                        <mo>=</mo>
                        <mrow>
                            <mo>λ</mo>
                            <mi>x</mi>
                            <mo>.</mo>
                            <mrow>
                                <mi>x</mi>
                                <mi>x</mi>
                            </mrow>
                        </mrow>
                    </mrow>
                </component>
            </component>
        </component>
    </math>
</template>

<script setup>
const { display } = defineProps({

    display: {
        type: String,
        default: 'inline',
    },
})
</script>