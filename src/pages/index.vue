<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="container">
        <img id="portrait" src="/kirk-small.png">
        <ExpandedRoutesList />
    </div>
</template>

<style scoped>
.container {
    display: flex;
    flex-flow: column nowrap;
    align-items: center;
    justify-content: start;
}

@media (width >=1200px) {

    .container {
        flex-flow: row nowrap;
    }
}
</style>

<script setup>
import ExpandedRoutesList from '@/components/ExpandedRoutesList.vue';
</script>