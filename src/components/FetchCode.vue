<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <highlightjs :code="code" :language="language" />
</template>

<script setup>
import { onMounted, ref } from 'vue'

const { url } = defineProps({

    url: {
        type: String,
        required: true,
    },

    language: {
        type: String,
        required: true,
    },
})

const code = ref(`awaiting ${url}`)

onMounted(async () => {

    try {

        const response = await fetch(url)

        if (response.status < 200 || response.status >= 300) {

            throw response.statusText
        }

        code.value = await response.text()

    } catch (error) {

        code.value = error
    }
})
</script>