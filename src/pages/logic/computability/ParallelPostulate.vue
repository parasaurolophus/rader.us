<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div class="columns">

        <div>
            <svg width="200" height="200" xmlns="http://www.w3.org/2000/svg">
                <line x1="20" y1="20" x2="180" y2="125" />
                <line x1="20" y1="180" x2="180" y2="75" />
                <line x1="50" y1="20" x2="50" y2="180" />
                <circle cx="142" cy="100" r="8" />
                <text x="44.5" y="13">A</text>
                <text x="44.5" y="197">B</text>
                <text x="57" y="70">α</text>
                <text x="57" y="140">β</text>
                <text x="136" y="82">C</text>
            </svg>
        </div>

        <div class="text">
            5. The point <i>C</i> is on the same side of line <i>AB</i> as
            angles <math><mn>α</mn></math> and <math><mn>β</mn></math> when both
            angles are less than 90°.
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