<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <div class="columns">

            <div>
                <h1>Music</h1>
                <RouteTree />
            </div>

            <hr v-if="!isLargeScreen">

            <div class="links">

                <div>
                    Choose a streaming service on which to hear my stuff&hellip;
                    at your own risk!
                </div>

                <ExternalLinksView v-model="links" size="x-large" />

                <p>
                    Or search for "<CopyableSpan>Kirk Rader</CopyableSpan>" on
                    the music service you prefer.
                </p>
            </div>

        </div>

    </div>

</template>

<style scoped>
@media (width >=1200px) {

    .columns {
        display: flex;
        flex-flow: row nowrap;
    }

    .links {
        display: flex;
        flex-flow: column nowrap;
        align-items: center;
        margin-left: auto;
        margin-right: auto;
    }
}
</style>

<script setup>
import CopyableSpan from '@/components/CopyableSpan.vue'
import ExternalLinksView from '@/components/ExternalLinksView.vue'
import RouteTree from '@/components/RouteTree'
import { inject, onMounted, onUnmounted, ref, watch } from 'vue'

let largeScreenQuery = null
const links = ref([])
const isLargeScreen = inject('isLargeScreen')
const musicLinks = inject('musicLinks')

function updateLinks() {

    links.value = Object.values(musicLinks.value)
}

onMounted(updateLinks)
watch(musicLinks, updateLinks)
</script>