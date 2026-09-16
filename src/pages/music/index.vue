<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <div class="columns">

            <div class="column">
                <h1>Music</h1>
                <RouteTree />
            </div>

            <div class="column">

                <div>
                    Choose a streaming service on which to hear my stuff&hellip;
                    at your own risk!
                </div>

                <ExternalLinksView v-model="links" size="x-large" />

                <div>
                    Or search for "<CopyableSpan>Kirk Rader</CopyableSpan>" on
                    the music service you prefer.
                </div>
            </div>

        </div>

    </div>

</template>

<style scoped>
.columns {
    display: grid;
    grid-template-columns: 48% 48%;
}
</style>

<script setup>
import CopyableSpan from '@/components/CopyableSpan.vue'
import ExternalLinksView from '@/components/ExternalLinksView.vue'
import RouteTree from '@/components/RouteTree'
import { inject, onMounted, ref, watch } from 'vue'

const musicLinks = inject('musicLinks')
const links = ref([])

function update() {

    links.value = Object.values(musicLinks.value)
}

onMounted(update)
watch(musicLinks, update)
</script>