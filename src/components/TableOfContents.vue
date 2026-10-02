<!-- Copyright (c) Kirk Rader -->

<template>
    <div class="container">
        <details ref="details">
            <summary>
                <MdiIcon :path="mdiMenu" />
            </summary>
            <ExpandedRoutesList />
        </details>
    </div>
</template>

<style scoped>
.container {
    width: max-content;
    padding-right: 1rem;
    margin-right: 1rem;
    background-color: var(--highlight);
}

summary {
    display: inline-block;
}
</style>

<script setup>
import ExpandedRoutesList from '@/components/ExpandedRoutesList.vue'
import MdiIcon from '@/components/MdiIcon.vue'
import { onMounted, onUnmounted, useTemplateRef } from 'vue'
import { useRouter } from 'vue-router'
import { mdiMenu } from '@mdi/js'

const details = useTemplateRef('details')
const router = useRouter()
let largeScreen = false
let windowWidthQuery = null

function onWindowWidthEvent(event) {

    windowWidthEventHandler(event.target)
}

function updateDetails() {

    details.value.open = largeScreen || router.currentRoute.value.name === 'home'
}

function windowWidthEventHandler(query) {

    largeScreen = query.matches
    updateDetails()
}

onMounted(() => {

    windowWidthQuery = window.matchMedia('(width >= 1200px)')
    windowWidthEventHandler(windowWidthQuery)
    windowWidthQuery.addEventListener('change', onWindowWidthEvent)
    router.afterEach(updateDetails)
})

onUnmounted(() => {

    if (windowWidthQuery !== null) {

        windowWidthQuery.removeEventListener('change', onWindowWidthEvent)
        windowWidthQuery = null
    }
})
</script>