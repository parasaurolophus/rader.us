<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>
    <template v-for="route of selectedRoutes" :key="route.path">
        <component :is="wrapper">
            <RouterLink :to="route.path">{{ route.meta?.title ?? route.name ?? route.path }}</RouterLink>
        </component>
    </template>
</template>

<script setup>
import { computed } from 'vue'
import { RouterLink, useRouter } from 'vue-router'

const { wrapper, pattern } = defineProps(['wrapper', 'pattern'])

const re = new RegExp(pattern)
const router = useRouter()

const selectedRoutes = computed(() => {

    return router.getRoutes()
        .filter(route => re.test(route.path))
        .sort((a, b) => a.path.localeCompare(b.path))
})
</script>