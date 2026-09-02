<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <math :display="display">
        <component :is="display === 'block' ? 'mtable' : 'mrow'">
            <component :is="display === 'block' ? 'mtr' : 'mrow'">
                <component :is="display === 'block' ? 'mtd' : 'mrow'">
                    <mi>&Omega;</mi>
                    <mo>&equals;</mo>
                    <mrow>
                        <mi>&omega;</mi>
                        <mi>&omega;</mi>
                    </mrow>
                </component>
            </component>
            <component :is="display === 'block' ? 'mtr' : 'mrow'">
                <component :is="display === 'block' ? 'mtd' : 'mrow'">
                    <template v-if="display === 'inline'">
                        <mtext>,</mtext>
                        <mspace width="0.5em" />
                    </template>
                    <mtext>where</mtext>
                    <mspace width="0.5em" />
                    <mrow>
                        <mi>&omega;</mi>
                        <mo>&equals;</mo>
                        <mrow>
                            <mo>&lambda;</mo>
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