<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <h1>Music</h1>

        <fieldset>
            <legend>here</legend>
            <RouteTree :root="route" />
        </fieldset>

        <fieldset>

            <legend>elsewhere</legend>

            <div>
                <MusicStreamer v-model="externalLinks.amazonMusic" />
                <MusicStreamer v-model="externalLinks.appleMusic" />
                <MusicStreamer v-model="externalLinks.spotify" />
                <MusicStreamer v-model="externalLinks.youtubeMusic" />
            </div>

            <div>
                <i>
                    ...or search for
                    "<CopyableSpan>Kirk Rader</CopyableSpan>"
                    on the music service you prefer
                </i>
            </div>

        </fieldset>

        <AmazonMusic />
        <AppleMusic />
        <SpotifyMusic />

    </div>

</template>

<script setup>
import AmazonMusic from '@/components/music/AmazonMusic.vue'
import AppleMusic from '@/components/music/AppleMusic.vue'
import CopyableSpan from '@/components/CopyableSpan.vue'
import MusicStreamer from '@/components/music/MusicStreamer.vue'
import RouteTree from '@/components/RouteTree'
import SpotifyMusic from '@/components/music/SpotifyMusic.vue'
import { useRoute } from 'vue-router'
import { inject } from 'vue'

const externalLinks = inject('externalLinks')
const route = useRoute()
</script>