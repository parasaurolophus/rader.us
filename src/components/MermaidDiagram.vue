<!-- Copyright (c) 2026 Kirk Rader -->

<template>
    <div ref="diagramElement">
        <slot></slot>
    </div>
</template>

<script setup>
import { computed, inject, onMounted, useTemplateRef, watch } from 'vue'
import { renderDiagram } from '@/mermaidUtilities.js'

const refreshDiagrams = inject('refreshDiagrams')
const diagramElement = useTemplateRef('diagramElement')
const diagramSource = computed(() => diagramElement?.value.innerText ?? `flowchart LR
n1@{ shape: braces, label: "uninitialized" }`)

const { svgId } = defineProps({

    svgId: {
        type: String,
        required: true,
    },
})

function renderModel() {

    renderDiagram(svgId, diagramSource.value, diagramElement.value)
}

onMounted(renderModel)
watch(refreshDiagrams, renderModel)
</script>