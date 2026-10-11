<!-- Copyright (c) 2026 Kirk Rader -->

<template>
    <a href="https://www.google.com/search?q=Undecidable+by+%22Kirk+Rader%22" target="_blank">
        <math :display="isLargeScreen ? 'inline' : 'block'">
            <component :is="isLargeScreen ? 'mrow' : 'mtable'">
                <component :is="isLargeScreen ? 'mrow' : 'mtr'">
                    <component :is="isLargeScreen ? 'mrow' : 'mtd'">
                        <mrow>
                            <mtext>Let</mtext>
                            <mspace width="0.5em" />
                            <mi>&Omega;</mi>
                            <mo>=</mo>
                            <mrow>
                                <mi>&omega;</mi>
                                <mspace width="0.5em" />
                                <mi>&omega;</mi>
                            </mrow>
                        </mrow>
                    </component>
                </component>
                <component :is="isLargeScreen ? 'mrow' : 'mtr'">
                    <component :is="isLargeScreen ? 'mrow' : 'mtd'">
                        <mspace v-if="isLargeScreen" width="0.5em" />
                        <mtext>where</mtext>
                        <mspace width="0.5em" />
                        <mrow>
                            <mi>&omega;</mi>
                            <mo>=</mo>
                            <mrow>
                                <mo>&lambda;</mo>
                                <mi>x</mi>
                                <mo>.</mo>
                                <mrow>
                                    <mi>x</mi>
                                    <mspace width="0.5em" />
                                    <mi>x</mi>
                                </mrow>
                            </mrow>
                        </mrow>
                    </component>
                </component>
            </component>
        </math>
    </a>
</template>

<script setup>
import { inject } from 'vue'

const isLargeScreen = inject('isLargeScreen')
</script>