<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <h1>Software Development</h1>

        <h2>Examples</h2>

        <details v-for="example in examples" :key="example.url" name="software">
            <summary>{{ example.summary }} ({{ example.language }})</summary>
            <highlightjs v-if="example.code" :code="example.code" :language="example.language" />
            <FetchCode v-if="example.url" :url="example.url" :language="example.language" />
            <a v-if="example.repository" :href="example.repository" target="_blank">
                {{ example.repository }}
            </a>
        </details>

    </div>

</template>

<script setup>
import FetchCode from '@/components/FetchCode.vue'

const examples = [
    {
        url: 'https://raw.githubusercontent.com/parasaurolophus/plug-in-favicon/refs/heads/main/plug-in-favicon.scm',
        summary: 'GIMP Plug-In',
        language: 'scheme',
        repository: 'https://github.com/parasaurolophus/plug-in-favicon',
    },
    {
        url: 'https://raw.githubusercontent.com/parasaurolophus/scheme/refs/heads/main/engines.scm',
        summary: 'Engines from Continuations',
        language: 'scheme',
        repository: 'https://github.com/parasaurolophus/scheme',
    },
]
</script>