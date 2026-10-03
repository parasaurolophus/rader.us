<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>

    <div>

        <h1>Musicography</h1>

        <QrLink v-model="hyperfollow" size="large" />

        <hr>

        <MusicographyView />

    </div>

</template>

<script setup>
import MusicographyView from '@/pages/music/musicography/MusicographyView.vue'
import QrLink from '@/components/QrLink.vue'
import { ref } from 'vue'

const hyperfollow = ref('https://hyperfollow.com/kirkrader')
</script>