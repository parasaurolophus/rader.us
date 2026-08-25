<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>
    <div>
        <h1>Music</h1>
        <ul>
            <MatchingRoutes wrapper="li" pattern="^/music/" />
        </ul>
    </div>
</template>

<script setup>
import MatchingRoutes from '@/components/MatchingRoutes.vue'
</script>