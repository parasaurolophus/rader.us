<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="container">
        <img id="portrait" src="/kirk.png">
        <RouteTree id="menu" />
    </div>
</template>

<style scoped>
#menu {
    text-wrap: nowrap;
}

@media (width < 600px) {

    .container {
        display: flex;
        flex-flow: column nowrap;
        align-items: center;
        justify-content: start;
    }

    #portrait {
        width: 50vw;
    }
}

@media (width >=600px) {

    .container {
        display: flex;
        flex-flow: row nowrap;
        align-items: center;
        justify-content: start;
    }

    #portrait {
        width: 20em;
    }
}
</style>

<script setup>
import RouteTree from '@/components/RouteTree'
</script>