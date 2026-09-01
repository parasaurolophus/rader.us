<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <EnterpriseSoftwareDevelopment />

        <h2>Parallel Postulate</h2>

        <div class="columns">
            <svg width="200" height="200" xmlns="http://www.w3.org/2000/svg">

                <line x1="20" y1="20" x2="180" y2="125" />
                <line x1="20" y1="180" x2="180" y2="75" />
                <line x1="50" y1="20" x2="50" y2="180" />
                <circle cx="142" cy="100" r="8" />
                <text x="44.5" y="13">A</text>
                <text x="44.5" y="197">B</text>
                <text x="57" y="70">&#x03b1;</text>
                <text x="57" y="140">&#x03b2;</text>
                <text x="136" y="82">C</text>
            </svg>
            <div>
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
}

.columns:not(:first-child)>* {
    width: max-content;
    max-width: 20em;
}
</style>

<script setup>
import EnterpriseSoftwareDevelopment from '@/components/software/EnterpriseSoftwareDevelopment.vue'
</script>