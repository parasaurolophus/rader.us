<!-- Copyright (c) Kirk Rader -->

<template>
    <div class="sidebar">
        <img id="portrait" src="/kirk.png">
        <RouteTree :root="root" />
    </div>
</template>

<style scoped>
.sidebar {
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
import RouteTree from '@/components/RouteTree'
import { useRouter } from 'vue-router'
import { computed } from 'vue'

const router = useRouter()
const root = computed(() => router.resolve({ name: 'home' }))
</script>