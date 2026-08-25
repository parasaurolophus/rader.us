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
import OmegaFormula from '@/components/OmegaFormula.vue'
import SideBar from '@/components/SideBar.vue'
import TitleBar from '@/components/TitleBar.vue'

function mermaidClick(arg) {

    console.log(`mermaid click ${arg}`)
}

mermaidHandler = mermaidClick

mermaid.initialize({
    startOnLoad: false,
    theme: 'dark',
    securityLevel: 'loose',
    htmlLabels: false,
})
</script>