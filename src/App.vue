<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>
    <header>
        <QrCode v-model="currentUrl" />
        <div id="breadcrumbs-wrapper">
            <div class="title">
                Kirk Rader
            </div>
            <BreadCrumbs />
        </div>
        <MdiIcon id="theme-button" :d="mdiThemeLightDark" @click.stop.prevent="toggleTheme" />
    </header>
    <div id="menu-wrapper">
        <TableOfContents />
        <main>
            <RouterView />
        </main>
    </div>
    <footer>
        <span>
            &copy; 2023-2026 Kirk Rader
        </span>
        <UndecidableLink v-if="isLargeScreen" />
    </footer>
</template>

<style scoped>
#breadcrumbs-wrapper {
    margin-left: 1rem;
}

footer {
    position: fixed;
    left: 0;
    bottom: 0;
    right: 0;
    height: var(--footer-height);
    display: flex;
    align-items: center;
    justify-content: space-around;
    padding-left: 1rem;
    padding-right: 1rem;
    background-color: var(--highlight-color);
}

header {
    position: fixed;
    left: 0;
    top: 0;
    right: 0;
    height: var(--header-height);
    display: flex;
    align-items: center;
    padding-left: 1rem;
    padding-right: 1rem;
    background-color: var(--highlight-color);
}

main {
    margin-top: 1ex;
    width: 100%;
    height: 100%;
    overflow: auto;
    padding-left: 1rem;
    padding-right: 1rem;
}

@media (width >=1200px) {

    main {
        margin-top: 0;
    }
}

#menu-wrapper {
    position: fixed;
    left: 0;
    top: var(--header-height);
    right: 0;
    bottom: var(--footer-height);
    display: flex;
    flex-flow: column nowrap;
    padding: 1rem;
}

@media (width >=1200px) {

    #menu-wrapper {
        flex-flow: row nowrap;
    }
}

#theme-button {
    margin-left: auto;
}

.title {
    font-size: large;
    font-weight: bold;
}
</style>

<script setup>
import BreadCrumbs from './components/BreadCrumbs.vue'
import MdiIcon from './components/MdiIcon.vue'
import QrCode from './components/QrCode.vue'
import TableOfContents from './components/TableOfContents.vue'
import mermaid from 'mermaid'
import { RouterView } from 'vue-router'
import { computed, onMounted, onUnmounted, provide, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { mdiThemeLightDark } from '@mdi/js'
import UndecidableLink from './components/UndecidableLink.vue'

const currentTheme = ref('dark-theme')
const currentUrl = computed(() => new URL(route.path, window.location).href)
const isLargeScreen = ref(false)
let largeScreenQuery = null

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
const route = useRoute()

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

function onLargeScreenChanged(query) {

    isLargeScreen.value = query.matches
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

onMounted(() => {

    largeScreenQuery = window.matchMedia('(width >= 1200px)')
    onLargeScreenChanged(largeScreenQuery)
    largeScreenQuery.addEventListener('change', onLargeScreenChanged)
    initializeMermaid()
})

onUnmounted(() => {

    largeScreenQuery.removeEventListener('change', onLargeScreenChanged)
})

watch(currentTheme, initializeMermaid)
</script>