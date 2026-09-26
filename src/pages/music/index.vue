<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <h1>Music</h1>

        <div>
            Choose a streaming service on which to hear my stuff:
        </div>

        <ExternalLinksView v-model="links" size="x-large" />

        <p>
            Or search for "<CopyableSpan>Kirk Rader</CopyableSpan>" on the music
            service you prefer.
        </p>
    </div>

</template>

<script setup>
import CopyableSpan from '@/components/CopyableSpan.vue'
import ExternalLinksView from '@/components/ExternalLinksView.vue'
import { inject, onMounted, ref, watch } from 'vue'

const links = ref([])
const musicLinks = inject('musicLinks')

function updateLinks() {

    links.value = Object.values(musicLinks.value)
}

onMounted(updateLinks)
watch(musicLinks, updateLinks)
</script>