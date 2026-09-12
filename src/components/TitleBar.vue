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
        <MdiIcon :path="mdiThemeLightDark" class="button right" @click="toggleTheme()" />
        <video autoplay loop muted disablepictureinpicture class="button" @click="toggleSidebar()">
            <source src="/logo64.webm" />
        </video>
    </div>
</template>

<style scoped>
.button {
    cursor: pointer;
    margin-left: 1rem;
}

#qr {
    margin-right: 1rem;
}

.right {
    margin-left: auto;
}

.title {
    font-size: xx-large;
}

.titlebar {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
}

@media print {
    #sidebar {
        display: none;
    }
}
</style>

<script setup>
import BreadCrumbs from '@/components/BreadCrumbs.vue'
import QrComponent from '@/components/QrComponent.vue'
import MdiIcon from '@/components/MdiIcon.vue'
import { mdiThemeLightDark } from '@mdi/js'
import { inject, onMounted } from 'vue'
import { useRouter } from 'vue-router'

const otherLinks = inject('otherLinks')
const router = useRouter()
const toggleTheme = inject('toggleTheme')

function toggleSidebar() {

    sidebar.style.display = sidebar.style.display === 'block' ? 'none' : 'block'
}

function hideSidebar() {

    sidebar.style.display = 'none'
}

onMounted(() => {

    router.afterEach(hideSidebar)
})
</script>