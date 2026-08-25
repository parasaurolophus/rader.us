<!-- Copyright (c) Kirk Rader -->

<template>
    <div class="container">
        <img id="portrait" src="/kirk.png">
        <fieldset>
            <legend>here</legend>
            <AllLinks />
        </fieldset>
        <fieldset>
            <legend>elsewhere</legend>
            <a href="https://hyperfollow.com/kirkrader" target="_blank">HyperFollow</a>
            <a href="https://github.com/parasaurolophus" target="_blank">GitHub</a>
        </fieldset>
    </div>
</template>

<style scoped>
.container {
    display: flex;
    flex-flow: column wrap;
    align-items: center;
}

#portrait {
    width: calc(0.9 * var(--sidebarwidth));
    height: calc(0.9 * var(--sidebarwidth));
}

fieldset {
    display: flex;
    flex-flow: row wrap;
    align-items: center;
    justify-content: space-around;
    border-style: solid;
    margin: 1vh 1vw;
}

fieldset>* {
    margin: 1vh 1vw;
}
</style>

<script setup>
import AllLinks from '@/components/AllLinks.vue'
</script>