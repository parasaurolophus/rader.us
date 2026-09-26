<!-- Copyright (c) 2026 Kirk Rader -->

<template>
    <pre ref="diagramElement"></pre>
</template>

<script setup>
import { inject, onMounted, useTemplateRef, watch } from 'vue'
import { renderDiagram } from '@/mermaidUtilities.js'

const diagramElement = useTemplateRef('diagramElement')
const refreshDiagrams = inject('refreshDiagrams')
const source = defineModel()

const { svgId } = defineProps({

    svgId: {
        type: String,
        required: true,
    },
})

function renderModel() {

    renderDiagram(svgId, source.value, diagramElement.value)
}

onMounted(renderModel)
watch(refreshDiagrams, renderModel)
watch(source, renderModel)
</script>