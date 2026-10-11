<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>

    <div>

        <QrLink v-model="github" size="large" />

        <div>

            <h1>Software Development</h1>

            <h2>Examples</h2>

            <details v-for="example in examples" :key="example.url" name="software">
                <summary>{{ example.summary }}</summary>
                <highlightjs v-if="example.code" :code="example.code" :language="example.language" />
                <FetchCode v-if="example.url" :url="example.url" :language="example.language" />
                <a v-if="example.repository" :href="example.repository" target="_blank">
                    {{ example.repository }}
                </a>
            </details>

        </div>

    </div>

</template>

<script setup>
import FetchCode from '@/components/FetchCode.vue'
import QrLink from '@/components/QrLink.vue'
import { ref } from 'vue'

const examples = [
    {
        url: 'https://raw.githubusercontent.com/parasaurolophus/automation/refs/heads/main/automation-backend/index.js',
        summary: 'Home automation server using express and Vue',
        language: 'javascript',
        repository: 'https://github.com/parasaurolophus/automation',
    },
    {
        url: 'https://raw.githubusercontent.com/parasaurolophus/plug-in-favicon/refs/heads/main/plug-in-favicon.scm',
        summary: 'GIMP plug-in (Scheme)',
        language: 'scheme',
        repository: 'https://github.com/parasaurolophus/plug-in-favicon',
    },
    {
        url: 'https://raw.githubusercontent.com/parasaurolophus/scheme/refs/heads/main/engines.rkt',
        summary: 'Engines from continuations (Scheme)',
        language: 'scheme',
        repository: 'https://github.com/parasaurolophus/scheme',
    },
    {
        url: 'https://raw.githubusercontent.com/parasaurolophus/go/refs/heads/main/utilities/process_batch.go',
        summary: 'goroutines and channels (Go)',
        language: 'go',
        repository: 'https://github.com/parasaurolophus/go/tree/main/utilities',
    },
    {
        url: 'https://raw.githubusercontent.com/parasaurolophus/cppExamples/refs/heads/main/src/Thread.h',
        summary: "Wrap Linux' pthread_t in a C++ class (C++)",
        language: 'cpp',
        repository: 'https://github.com/parasaurolophus/cppExamples',
    },
]

const github = ref('https://github.com/parasaurolophus')
</script>