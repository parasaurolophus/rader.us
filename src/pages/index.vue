<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="container">
        <img id="portrait" src="/kirk-small.png">
        <RouteTree id="menu" />
    </div>
</template>

<style scoped>
#menu {
    text-wrap: nowrap;
}

.container {
    display: flex;
    align-items: center;
    flex-flow: column nowrap;
    justify-content: start;
}

@media (width >=1200px) {

    .container {
        flex-flow: row nowrap;
        justify-content: center;
    }

    #menu {
        margin-right: auto;
    }

    #portrait {
        margin-left: auto;
    }
}
</style>

<script setup>
import RouteTree from '@/components/RouteTree'
</script>