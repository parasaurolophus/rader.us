<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <h1>404</h1>

    <p>
        Hmmm&hellip; it seems that {{ url }} does not exist.
    </p>

    <p>
        Maybe you actually <em>can</em> go
        <router-link :to="{ name: 'home' }">home</router-link>
        again, after all?
    </p>

</template>

<script setup>
import { computed } from 'vue'
import { useRoute } from 'vue-router'

const route = useRoute()
const url = computed(() => new URL(route.fullPath, import.meta.url))
</script>