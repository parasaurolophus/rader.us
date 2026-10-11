<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>

    <span>
        © 2023-2026 Kirk Rader
    </span>

    <UndecidableLink v-if="isLargeScreen" />
</template>

<script setup>
import UndecidableLink from '@/components/UndecidableLink.vue'
import { inject } from 'vue'

const isLargeScreen = inject('isLargeScreen')
</script>