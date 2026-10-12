<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>
    <div ref="svg" :class="`container ${size}`">
    </div>
</template>

<style scoped>
.container {
    display: inline-block;
}

.small {
    width: 28px;
}

.medium {
    width: 56px;
}

.large {
    width: 112px;
}

.x-large {
    width: 224px;
}
</style>

<script setup>
import encodeQR from 'qr'
import { onMounted, useTemplateRef, watch } from 'vue'

const { size, text } = defineProps({

    size: {
        type: String,
        default: 'medium',
    },

    text: {
        type: String,
        required: true,
    },
})

const svg = useTemplateRef('svg')

function update() {

    svg.value.innerHTML = encodeQR(text, 'svg')
}

onMounted(update)
</script>