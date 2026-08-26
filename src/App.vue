<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <header>
        <div class="padded">
            <TitleBar />
        </div>
    </header>
    <main>
        <div class="padded">
            <RouterView />
        </div>
    </main>
    <div id="sidebar">
        <div class="padded">
            <SideBar />
        </div>
    </div>
    <footer>
        <div class="padded footer">
            <span>
                &copy; Kirk Rader 2023-2026
            </span>
            <OmegaFormula />
        </div>
    </footer>
</template>

<style scoped>
.padded {
    padding: 0.5em;
}

.footer {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
    justify-content: space-around;
}
</style>

<script setup>
import mermaid from 'mermaid'
import { RouterView } from 'vue-router'
import { provide, ref } from 'vue'
import OmegaFormula from '@/components/OmegaFormula.vue'
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
    github: {
        title: 'GitHub',
        url: 'https://github.com/parasaurolophus',
    },
    hyperFollow: {
        title: 'HyperFollow',
        url: 'https://hyperfollow.com/kirkrader',
    },
    spotify: {
        title: 'Spotify',
        url: 'https://open.spotify.com/artist/06lMz4EjJn3pYej2kGIL5t',
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