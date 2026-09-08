<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="titlebar">
        <a id="qr" :href="otherLinks.hyperFollow.url" target="_blank">
            <QrComponent v-model="otherLinks.hyperFollow.url" />
        </a>
        <div>
            <div class="title">
                Kirk Rader
            </div>
            <BreadCrumbs />
        </div>
        <MdiIcon id="theme-button" :path="mdiThemeLightDark" class="button" @click="toggleTheme()" />
        <MdiIcon :path="mdiMenu" class="button" @click="toggleSidebar()" />
    </div>
</template>

<style scoped>
.button {
    cursor: pointer;
    margin-left: 1em;
}

#qr {
    margin-right: 1em;
}

.title {
    font-size: xx-large;
}

.titlebar {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
}

#theme-button {
    margin-left: auto;
}
</style>

<script setup>
import BreadCrumbs from '@/components/BreadCrumbs.vue'
import QrComponent from '@/components/QrComponent.vue'
import MdiIcon from '@/components/MdiIcon.vue'
import { mdiThemeLightDark, mdiMenu } from '@mdi/js'
import { inject } from 'vue'

const otherLinks = inject('otherLinks')
const toggleTheme = inject('toggleTheme')

function toggleSidebar() {

    const sidebar = document.getElementById('sidebar')

    sidebar.style.display = sidebar.style.display === 'block' ? 'none' : 'block'
}
</script>