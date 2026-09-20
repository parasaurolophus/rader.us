<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <details name="details-view" class="details-view">
        <summary>
            <slot name="summary"></slot>
        </summary>
        <slot></slot>
    </details>
</template>

<style>
.details-view {

    padding: 1rem;
    font-style: italic;
    font-size: 90%;
}

.details-view:open {

    border: solid 1px var(--foreground);
}

.details-view>*:not(summary) {

    margin-left: 1rem;
}

.details-view>summary {

    font-weight: bold;
    cursor: pointer;
}

.details-view>summary,
.details-view i,
.details-view em,
.details-view math {

    font-style: normal;
    font-weight: bold;
}
</style>