<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <header>
        <TitleBar />
    </header>
    <main>
        <div id="sidebar">
            <SideBar />
        </div>
        <div id="current-page">
            <RouterView />
        </div>
    </main>
    <footer>
        <FooterBar />
    </footer>
</template>

<style scoped>
#current-page {
    margin-left: 0;
}

#sidebar {

    border-right: none;
}

@media (width >=1200px) {

    #current-page {
        margin-left: 1rem;
    }

    #sidebar {
        border-right: solid;
    }
}
</style>

<script setup>
import FooterBar from './components/FooterBar.vue'
import SideBar from '@/components/SideBar.vue'
import TitleBar from '@/components/TitleBar.vue'
import mermaid from 'mermaid'
import { RouterView } from 'vue-router'
import { onMounted, provide, ref, watch } from 'vue'

const currentTheme = ref('dark-theme')
const isLargeScreen = ref(false)
const largeScreenQuery = window.matchMedia('(width >= 1200px)')

isLargeScreen.value = largeScreenQuery.matches
largeScreenQuery.addEventListener('change', onLargeScreenChanged)

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
    github: {
        title: 'GitHub',
        url: 'https://github.com/parasaurolophus',
    },
})

const refreshDiagrams = ref(0)

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

function onLargeScreenChanged(event) {

    isLargeScreen.value = event.target.matches
}

function toggleTheme() {

    const newTheme = currentTheme.value === 'dark-theme' ? 'light-theme' : 'dark-theme'

    document.getElementsByTagName('body')[0].classList.replace(currentTheme.value, newTheme)
    currentTheme.value = newTheme
}

provide('currentTheme', currentTheme)
provide('isLargeScreen', isLargeScreen)
provide('musicLinks', musicLinks)
provide('otherLinks', otherLinks)
provide('refreshDiagrams', refreshDiagrams)
provide('toggleTheme', toggleTheme)

mermaidHandler = mermaidClick

onMounted(initializeMermaid)
watch(currentTheme, initializeMermaid)
</script>