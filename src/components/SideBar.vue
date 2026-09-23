<!-- Copyright (c) Kirk Rader -->

<template>
    <div class="container">
        <details ref="details">
            <summary>
                Table of Contents
            </summary>
            <ExpandedRoutesList />
        </details>
    </div>
</template>

<style scoped>
.container {
    width: max-content;
    margin-right: 1rem;
}

summary {
    cursor: pointer;
}
</style>

<script setup>
import ExpandedRoutesList from '@/components/ExpandedRoutesList.vue'
import { onMounted, onUnmounted, useTemplateRef } from 'vue'
import { useRoute, useRouter } from 'vue-router'

const details = useTemplateRef('details')
const router = useRouter()
let windowWidthQuery = null

function onWindowWidthEvent(event) {

    windowWidthEventHandler(event.target)
}

function updateDetails() {

    const route = useRoute()

    if (route.name === 'home') {

        details.value.open = true
    }
}

function windowWidthEventHandler(query) {

    details.value.open = query.matches
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