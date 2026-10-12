<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>

    <div>

        <h1>Musicography</h1>

        <QrLink :url="hyperfollow" size="large" />

        <hr>

        <MusicographyView />

    </div>

</template>

<script setup>
import MusicographyView from './MusicographyView.vue'
import QrLink from '../../../components/QrLink.vue'

const hyperfollow = 'https://hyperfollow.com/kirkrader'
</script>