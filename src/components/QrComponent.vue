<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="svg">
        <svg ref="svg"></svg>
    </div>
</template>

<style scoped>
.svg {
    display: inline-block;
    color: black;
    width: 64px;
    background-color: white;
    border-radius: 0;
}
</style>

<script setup>
import encodeQR from 'qr'
import { onMounted, useTemplateRef, watch } from 'vue'

const svg = useTemplateRef('svg')
const text = defineModel()

function update() {

    svg.value.outerHTML = encodeQR(text.value, 'svg')
}

onMounted(update)
watch(text, update)
</script>