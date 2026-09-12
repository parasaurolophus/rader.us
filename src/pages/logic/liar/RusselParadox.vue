<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <math display="block">
        <component :is="display === 'block' ? 'mtable' : 'mrow'">
            <component :is="display === 'block' ? 'mtr' : 'mrow'">
                <component :is="display === 'block' ? 'mtd' : 'mrow'">
                    <mrow>
                        <mi>S</mi>
                        <mo>∈</mo>
                        <mi>S</mi>
                    </mrow>
                    <mo>↔</mo>
                    <mrow>
                        <mi>S</mi>
                        <mo>∉</mo>
                        <mi>S</mi>
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
                        <mi>S</mi>
                        <mo>=</mo>
                        <mo>{</mo>
                        <mi>s</mi>
                        <mo>:</mo>
                        <mrow>
                            <mi>s</mi>
                            <mo>∉</mo>
                            <mi>s</mi>
                        </mrow>
                        <mo>}</mo>
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