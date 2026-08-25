<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <h1>Music</h1>

        <RouteTree :root="route" />


        <DetailsComponent>

            <template #summary>
                Amazon Music
            </template>

            <iframe id='AmazonMusicEmbedB004L4HW52'
                src='https://music.amazon.com/embed/B004L4HW52/?id=q7SieUCVuT&marketplaceId=ATVPDKIKX0DER&musicTerritory=US'
                width='100%' height='352px' frameBorder='0' style='border-radius:20px;max-width:'>
            </iframe>

            <a href="https://music.amazon.com/artists/B004L4HW52/kirk-rader" target="_blank">
                https://music.amazon.com/artists/B004L4HW52/kirk-rader
            </a>
        </DetailsComponent>

        <DetailsComponent>

            <template #summary>
                Apple Music
            </template>

            <iframe allow="autoplay *; encrypted-media *;" frameborder="0" height="450"
                style="width:100%;max-width:660px;overflow:hidden;background:transparent;"
                sandbox="allow-forms allow-popups allow-same-origin allow-scripts allow-storage-access-by-user-activation allow-top-navigation-by-user-activation"
                src="https://embed.music.apple.com/us/artist/kirk-rader/417090159">
            </iframe>

            <a href="https://music.apple.com/us/artist/kirk-rader/417090159" target="_blank">
                https://music.apple.com/us/artist/kirk-rader/417090159
            </a>
        </DetailsComponent>

        <DetailsComponent>

            <template #summary>
                Spotify
            </template>

            <iframe style="border-radius:12px"
                src="https://open.spotify.com/embed/artist/06lMz4EjJn3pYej2kGIL5t?utm_source=generator" width="100%"
                height="352" frameBorder="0" allowfullscreen=""
                allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture" loading="lazy">
            </iframe>

            <a href="https://open.spotify.com/artist/06lMz4EjJn3pYej2kGIL5t" target="_blank">
                https://open.spotify.com/artist/06lMz4EjJn3pYej2kGIL5t
            </a>
        </DetailsComponent>

        <DetailsComponent>

            <template #summary>
                YouTube Music
            </template>

            <a href="https://music.youtube.com/channel/UCp__q4DYBXYhq9uiD2Y8vUg" target="_blank">
                <QrComponent v-model="externalLinks.youtubeMusic" />
                <br>
                {{ externalLinks.youtubeMusic }}
            </a>
        </DetailsComponent>

        <i>...or search for "Kirk Rader" on the music service you prefer</i>

    </div>

</template>

<script setup>
import DetailsComponent from '@/components/DetailsComponent.vue'
import QrComponent from '@/components/QrComponent.vue'
import RouteTree from '@/components/RouteTree'
import { inject } from 'vue'
import { useRoute } from 'vue-router'

const externalLinks = inject('externalLinks')
const route = useRoute()
</script>