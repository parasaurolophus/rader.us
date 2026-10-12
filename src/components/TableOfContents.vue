<!-- Copyright (c) Kirk Rader -->

<template>
    <details ref="details">
        <summary>
            <MdiIcon :d="mdiMenu" />
        </summary>
        <RouteTree :root="root" :links="links" />
        <template v-if="!isLargeScreen">
            <hr>
            <UndecidableLink />
        </template>
    </details>
</template>

<style scoped>
details {
    border-style: none;
}

details:open {
    background-color: var(--highlight-color);
}

summary {
    display: inline;
}
</style>

<script setup>
import MdiIcon from './MdiIcon.vue'
import RouteTree from './RouteTree.js'
import UndecidableLink from './UndecidableLink.vue'
import { computed, inject, onMounted, useTemplateRef, watch } from 'vue'
import { useRouter } from 'vue-router'
import { mdiMenu } from '@mdi/js'

const details = useTemplateRef('details')
const isLargeScreen = inject('isLargeScreen')
const otherLinks = inject('otherLinks')
const router = useRouter()
const root = computed(() => router.resolve({ name: 'home' }))
const links = computed(() => Object.values(otherLinks.value))

function updateDetails() {

    details.value.open = isLargeScreen.value || router.currentRoute.value.name === 'home'
}

onMounted(() => router.afterEach(updateDetails))
watch(isLargeScreen, updateDetails)
</script>