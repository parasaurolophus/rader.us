<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <h1>IEEE 754 Division by Zero</h1>

        <p>
            Division by zero of most numbers is well defined, and produces a
            result of &pm;&infin; because:
        </p>

        <DivisionByZero />

        <p>
            since the latter is the algebraic inverse of the former.
        </p>

        <p>
            However:
        </p>

        <ZeroOverZero />

        <p>
            due to conflicting axioms.
        </p>

        <ul>
            <li>
                On the one hand, dividing any number by 0 ought to produce an
                infinite quotient, as previously discussed.
            </li>
            <li>
                On the other hand, any number divided by itself ought to produce
                a quotient of 1 according to the axioms of arithmetic.
            </li>
        </ul>

        <p>
            Since <InlineZeroOverZero /> cannot be both 1 and &infin;, it must
            be neither. Because there are no other possibilities, the actual
            result is <code>NaN</code> ("not a number").
        </p>

        <p>
            The IEEE specification allows implementations to generate an OS trap
            or throw an excaption instead of returning <code>NaN</code>. The
            same applies in other cases where <code>NaN</code> would be
            returned, such as:
        </p>

        <NaN />

    </div>

</template>

<script setup lang="ts">
import DivisionByZero from '@/pages/logic/ieee754/DivisionByZero.vue'
import NaN from '@/pages/logic/ieee754/NaN.vue'
import ZeroOverZero from '@/pages/logic/ieee754/ZeroOverZero.vue'
import InlineZeroOverZero from './InlineZeroOverZero.vue';
</script>