<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <MdiIcon :path="mdiThemeLightDark" @click="toggleTheme()" />
</template>

<script setup>
import { inject, onMounted, watch } from 'vue'
import { mdiThemeLightDark } from '@mdi/js'
import MdiIcon from './MdiIcon.vue'

const currentTheme = inject('currentTheme')
const toggleTheme = inject('toggleTheme')

function update() {

}

onMounted(update)
watch(currentTheme, update)
</script>