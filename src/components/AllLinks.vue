<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <template v-for="route of namedRoutes" :key="route.name">
        <RouterLink :to="route.path">
            {{ route.meta?.title ?? route.name }}
        </RouterLink>
    </template>
</template>

<script setup>
import { computed } from 'vue'
import { RouterLink, useRouter } from 'vue-router'

const router = useRouter()
const namedRoutes = computed(() => router.getRoutes().filter(route => route.name ? true : false).sort(compareRoutes))

function compareRoutes(a, b) {

    // if (a.name === 'home') {

    //     return b.name === 'home' ? 0 : -1
    // }

    // if (b.name === 'home') {

    //     return 1
    // }

    return a.path.localeCompare(b.path)
}
</script>