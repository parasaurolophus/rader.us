<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <strong>This Statement Is False</strong>

        <div class="columns">

            <div>
                <YCombinator />
            </div>

            <p>
                <RouterLink :to="{ name: 'liar' }">
                    and
                </RouterLink>
            </p>

            <div>
                <RusselParadox />
            </div>

        </div>

        <hr>

        <strong>How Software Is Done</strong>

        <EnterpriseSoftwareDevelopment />

        <hr>

        <strong>The Postulate That Changed the World</strong>

        <div class="columns">
            <div>
                <ParallelPostulate />
            </div>
            <div class="text">
                The point <i>C</i> is on the same side of line <i>AB</i> as
                angles <i>&alpha;</i> and <i>&beta;</i> when both angles are
                less than 90&deg;.
            </div>
        </div>

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
import EnterpriseSoftwareDevelopment from '@/components/software/EnterpriseSoftwareDevelopment.vue'
import ParallelPostulate from '@/components/logic/ParallelPostulate.vue'
import RusselParadox from '@/components/logic/RusselParadox.vue'
import YCombinator from '@/components/logic/YCombinator.vue'
</script>