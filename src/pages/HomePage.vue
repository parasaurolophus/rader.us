<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <h1>Home</h1>

    <h2>Links</h2>

    <div class="links">
        <MatchingRoutes wrapper="span" pattern="^/[^/]+$" />
    </div>

    <DetailsComponent>

        <template #summary>
            IEEE 754 Division by Zero
        </template>

        <p>
            Division of most numbers by zero is well defined, and produces a
            result of &pm;&infin; because:
        </p>

        <math display="block">
            <LimitZero />
            <mo>&equiv;</mo>
            <LimitInfinity />
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

    </DetailsComponent>

</template>

<style scoped>
.links {
    display: flex;
    flex-flow: row wrap;
}

.links>* {
    margin: 0.25vh 0.25vw;
    padding: 0.25vh 0.25vw;
}
</style>

<script setup>
import DetailsComponent from '@/components/DetailsComponent.vue'
import LimitInfinity from '@/components/LimitInfinity.vue';
import LimitZero from '@/components/LimitZero.vue'
import MatchingRoutes from '@/components/MatchingRoutes.vue'
</script>