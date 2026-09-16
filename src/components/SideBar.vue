<!-- Copyright (c) Kirk Rader -->

<template>
    <div class="sidebar">
        <video autoplay loop muted>
            <source src="/logo256.webm" />
        </video>
        <ExpandedRoutesList />
    </div>
</template>

<style scoped>
.sidebar {
    margin-right: 1rem;
}
</style>

<script setup>
import ExpandedRoutesList from '@/components/ExpandedRoutesList.vue'
</script>