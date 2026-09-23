<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div>
        <div class="title">
            Kirk Rader
        </div>
        <BreadCrumbs />
    </div>
    <MdiIcon :path="mdiThemeLightDark" class="button right" @click="toggleTheme()" />
    <QrComponent id="qr" v-model="otherLinks.hyperFollow.url" />
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
</style>

<script setup>
import BreadCrumbs from '@/components/BreadCrumbs.vue'
import QrComponent from '@/components/QrComponent.vue'
import MdiIcon from '@/components/MdiIcon.vue'
import { mdiThemeLightDark } from '@mdi/js'
import { inject, onMounted } from 'vue'
import { useRouter } from 'vue-router'

const otherLinks = inject('otherLinks')
const toggleTheme = inject('toggleTheme')
</script>