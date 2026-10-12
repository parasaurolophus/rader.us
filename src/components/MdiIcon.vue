<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>
    <svg :width="24" :height="24" :view-box="`0 0 23 23`" :transform="`scale(${props.scale})`">
        <path :d="props.d" />
    </svg>
</template>

<style scoped>
svg {
    display: inline-block;
}

path {
    fill: currentColor;
    stroke: rgb(from currentColor r g b / 50%);
}
</style>

<script setup>
const props = defineProps({

    d: {
        type: String,
        required: true,
    },

    scale: {
        type: Number,
        default: 1,
    },
})
</script>