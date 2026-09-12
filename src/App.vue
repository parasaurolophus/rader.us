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

<script setup>
import FooterBar from './components/FooterBar.vue'
import SideBar from '@/components/SideBar.vue'
import TitleBar from '@/components/TitleBar.vue'
import mermaid from 'mermaid'
import { RouterView, useRoute, useRouter } from 'vue-router'
import { onMounted, provide, ref, watch } from 'vue'

const currentTheme = ref('dark-theme')

const musicLinks = ref({
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
    youtubeMusic: {
        title: 'YouTube Music',
        url: 'https://music.youtube.com/channel/UCp__q4DYBXYhq9uiD2Y8vUg',
    },
})

const otherLinks = ref({
    hyperFollow: {
        title: 'HyperFollow',
        url: 'https://hyperfollow.com/kirkrader',
    },
    undecidable: {
        title: 'Undecidable',
        url: 'https://music.apple.com/us/album/undecidable-ep/417090158?itscg=30200&itsct=music_box_link&ls=1&app=music&mttnsubad=417090158',
    },
})

const refreshDiagrams = ref(0)

const softwareLinks = ref({
    github: {
        title: 'GitHub',
        url: 'https://github.com/parasaurolophus',
    },
})

function initializeMermaid() {

    mermaid.initialize({
        startOnLoad: false,
        theme: currentTheme.value === 'dark-theme' ? 'dark' : 'default',
        securityLevel: 'loose',
        htmlLabels: false,
    })

    refreshDiagrams.value += 1
}

function mermaidClick(arg) {

    console.log(`mermaid click ${arg}`)
}

function toggleTheme() {

    const newTheme = currentTheme.value === 'dark-theme' ? 'light-theme' : 'dark-theme'

    document.getElementsByTagName('body')[0].classList.replace(currentTheme.value, newTheme)
    currentTheme.value = newTheme
}

provide('currentTheme', currentTheme)
provide('musicLinks', musicLinks)
provide('otherLinks', otherLinks)
provide('refreshDiagrams', refreshDiagrams)
provide('softwareLinks', softwareLinks)
provide('toggleTheme', toggleTheme)

mermaidHandler = mermaidClick

onMounted(initializeMermaid)

watch(currentTheme, initializeMermaid)
</script>