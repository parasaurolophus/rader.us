<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div>
        <h1>Logic</h1>
        <RouteTree :root="route" />
    </div>
</template>

<script setup>
import RouteTree from '@/components/RouteTree'
import { useRoute } from 'vue-router'

const route = useRoute()
</script>