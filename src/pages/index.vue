<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <strong>Ummm... Errr... Music?</strong>

        <p>
            <RouterLink :to="{ name: 'algorithmic' }">
                No, really...
            </RouterLink>
        </p>

        <strong>This Statement Is False</strong>

        <div class="columns">

            <RouterLink :to="{ name: 'liar' }"> What</RouterLink>

            <div>
                <YCombinator />
            </div>

            <RouterLink :to="{ name: 'liar' }">and</RouterLink>

            <div>
                <RusselParadox />
            </div>

            <RouterLink :to="{ name: 'liar' }">have in common</RouterLink>

        </div>

        <strong>The Axiom That Changed the World</strong>

        <RouterLink :to="{ name: 'computability' }">
            <ParallelPostulate />
        </RouterLink>

    </div>

</template>

<style scoped>
.columns {
    display: flex;
    align-items: center;
    justify-content: start;
}

.columns>* {
    display: inline-block;
    width: max-content;
    margin: 0.5em;
    padding: 0.5em;
}

.text {
    max-width: 20em;
}
</style>

<script setup>
import ParallelPostulate from '@/pages/logic/computability/ParallelPostulate.vue'
import RusselParadox from '@/pages/logic/liar/RusselParadox.vue'
import YCombinator from '@/pages/logic/liar/YCombinator.vue'
</script>