<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>
    <div v-if="isLargeScreen">
        <img src="/kirk-small.webp">
        <p>
            Welcome
        </p>
    </div>
</template>

<script setup>
import { inject } from 'vue'

const isLargeScreen = inject('isLargeScreen')
</script>