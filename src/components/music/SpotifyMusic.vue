<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <DetailsComponent>
        <template #summary>Spotify</template>
        <iframe style="border-radius:12px"
            src="https://open.spotify.com/embed/artist/06lMz4EjJn3pYej2kGIL5t?utm_source=generator" width="100%"
            height="352" frameBorder="0" allowfullscreen="false"
            allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture" loading="lazy">
        </iframe>
    </DetailsComponent>
</template>

<script setup>
import DetailsComponent from '@/components/DetailsComponent.vue'
</script>