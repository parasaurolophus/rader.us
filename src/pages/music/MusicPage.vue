<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <h1>Music</h1>

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

</template>

<script setup>
import CopyableSpan from '@/components/CopyableSpan.vue'
import ExternalLinksView from '@/components/ExternalLinksView.vue'
import { inject, onMounted, ref, watch } from 'vue'

const externalLinks = inject('externalLinks')
const links = ref([])

function update() {

    links.value = Object.values(externalLinks.value)
}

onMounted(update)
watch(externalLinks, update)
</script>