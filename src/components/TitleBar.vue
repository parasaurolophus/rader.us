<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="titlebar">
        <video autoplay loop muted disablepictureinpicture class="button" @click.stop.prevent="toggleSidebar()">
            <source src="/logo64.webm" />
        </video>
        <div>
            <div class="title">
                Kirk Rader
            </div>
            <BreadCrumbs />
        </div>
        <MdiIcon :path="mdiThemeLightDark" class="button right" @click="toggleTheme()" />
        <QrComponent id="qr" v-model="otherLinks.hyperFollow.url" />
    </div>
</template>

<style scoped>
.button {
    cursor: pointer;
}

#qr {
    margin: 0 1rem;
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
    justify-content: start;
    height: 100%;
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

const hideSidebar = inject('hideSidebar')
const otherLinks = inject('otherLinks')
const router = useRouter()
const toggleTheme = inject('toggleTheme')

function toggleSidebar() {

    sidebar.style.display = sidebar.style.display === 'block' ? 'none' : 'block'
}

onMounted(() => {

    router.afterEach(hideSidebar)
})
</script>