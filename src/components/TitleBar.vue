<!-- Copyright (c) 2026 Kirk Rader -->

<template>
    <QrComponent id="qr" v-model="route.path" />
    <div>
        <div class="title">
            Kirk Rader
        </div>
        <BreadCrumbs />
    </div>
    <MdiIcon :path="mdiThemeLightDark" class="button right" @click="toggleTheme()" />
    <QrComponent id="hyperfollow" v-model="otherLinks.hyperFollow.url" />
</template>

<style scoped>
.button {
    cursor: pointer;
}

#qr,
#hyperfollow {
    margin: 0 1rem;
}

.right {
    margin-left: auto;
}

.title {
    font-size: xx-large;
}
</style>

<script setup>
import BreadCrumbs from '@/components/BreadCrumbs.vue'
import QrComponent from '@/components/QrComponent.vue'
import MdiIcon from '@/components/MdiIcon.vue'
import { mdiThemeLightDark } from '@mdi/js'
import { inject } from 'vue'
import { useRoute } from 'vue-router'

const otherLinks = inject('otherLinks')
const route = useRoute()
const toggleTheme = inject('toggleTheme')
</script>