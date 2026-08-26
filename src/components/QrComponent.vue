<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div ref="svg" :class="`svg ${size}`"></div>
</template>

<style scoped>
.svg {
    display: inline-block;
    color: black;
    width: 64px;
    background-color: white;
    border-radius: 0;
}

.small {
    width: 32px;
}

.medium {
    width: 64px;
}

.large {
    width: 128px;
}
</style>

<script setup>
import encodeQR from 'qr'
import { onMounted, useTemplateRef, watch } from 'vue'

const { size } = defineProps({

    size: {
        type: String,
        default: 'medium',
    },
})

const svg = useTemplateRef('svg')
const text = defineModel()

function update() {

    svg.value.innerHTML = encodeQR(text.value, 'svg')
}

onMounted(update)
watch(text, update)
</script>