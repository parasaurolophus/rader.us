<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <header>
        <TitleBar />
    </header>
    <main>
        <RouterView />
    </main>
    <div id="sidebar">
        <SideBar />
    </div>
    <footer>
        <FooterBar />
    </footer>
</template>

<style scoped>
header>*,
main>*,
#sidebar>*,
footer>* {
    padding: 0.5em;
}
</style>

<script setup>
import mermaid from 'mermaid'
import { RouterView } from 'vue-router'
import { provide, ref } from 'vue'
import FooterBar from './components/FooterBar.vue'
import SideBar from '@/components/SideBar.vue'
import TitleBar from '@/components/TitleBar.vue'

const externalLinks = ref({
    amazonMusic: {
        title: 'Amazon Music',
        url: 'https://music.amazon.com/artists/B004L4HW52/kirk-rader',
    },
    appleMusic: {
        title: 'Apple Music',
        url: 'https://music.apple.com/us/artist/kirk-rader/417090159',
    },
    deezer: {
        title: 'Deezer',
        url: 'https://www.deezer.com/us/artist/5223459',
    },
    github: {
        title: 'GitHub',
        url: 'https://github.com/parasaurolophus',
    },
    hyperFollow: {
        title: 'HyperFollow',
        url: 'https://hyperfollow.com/kirkrader',
    },
    iheartradio: {
        title: 'iHeartRadio',
        url: 'https://www.iheart.com/artist/kirk-rader-539364',
    },
    pandora: {
        title: 'Pandora',
        url: 'https://www.pandora.com/artist/kirk-rader/ARd5ht9vgv6kzzc',
    },
    spotify: {
        title: 'Spotify',
        url: 'https://open.spotify.com/artist/06lMz4EjJn3pYej2kGIL5t',
    },
    tidal: {
        title: 'Tidal',
        url: 'https://tidal.com/artist/37978550',
    },
    undecidable: {
        title: 'Undecidable',
        url: 'https://music.apple.com/us/album/undecidable-ep/417090158?itscg=30200&itsct=music_box_link&ls=1&app=music&mttnsubad=417090158',
    },
    youtubeMusic: {
        title: 'YouTube Music',
        url: 'https://music.youtube.com/channel/UCp__q4DYBXYhq9uiD2Y8vUg',
    },
})

function mermaidClick(arg) {

    console.log(`mermaid click ${arg}`)
}

provide('externalLinks', externalLinks)

mermaidHandler = mermaidClick

mermaid.initialize({
    startOnLoad: false,
    theme: 'dark',
    securityLevel: 'loose',
    htmlLabels: false,
})
</script>