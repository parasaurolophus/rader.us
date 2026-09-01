<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div>
        <h1>Logic</h1>
        <RouteTree :links="links" />
    </div>
</template>

<script setup>
import RouteTree from '@/components/RouteTree'
import { computed, inject } from 'vue'

const softwareLinks = inject('softwareLinks')
const links = computed(() => Object.values(softwareLinks.value))

</script>