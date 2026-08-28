<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <h1>Music</h1>

        <div class="columns">

            <div class="column">
                <RouteTree />
            </div>

            <div class="column">
                <p>
                    <ExternalLinksView v-model="links" size="x-large" />
                </p>
                <p>
                    <i>
                        ...or search for
                        "<CopyableSpan>Kirk Rader</CopyableSpan>"
                        on the music service you prefer
                    </i>
                </p>
            </div>

        </div>

    </div>

</template>

<style scoped>
.columns {
    display: grid;
    grid-template-columns: max-content max-content;
}

.column {
    display: flex;
    flex-flow: column wrap;
    align-items: center;
    margin: 0 1em;
}
</style>

<script setup>
import CopyableSpan from '@/components/CopyableSpan.vue'
import ExternalLinksView from '@/components/ExternalLinksView.vue'
import RouteTree from '@/components/RouteTree'
import { inject, onMounted, ref, watch } from 'vue'

const musicLinks = inject('musicLinks')
const otherLinks = inject('otherLinks')
const links = ref([])

function update() {

    links.value = Object.values(musicLinks.value)
}

onMounted(update)
watch(musicLinks, update)
</script>