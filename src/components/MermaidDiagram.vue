<!-- Copyright (c) 2026 Kirk Rader -->

<template>
    <div ref="diagramElement">
        <slot></slot>
    </div>
</template>

<script setup>
import { computed, onMounted, useTemplateRef } from 'vue'
import mermaid from 'mermaid'

const diagramElement = useTemplateRef('diagramElement')
const diagramSource = computed(() => diagramElement?.value.innerText ?? `flowchart LR
n1@{ shape: braces, label: "uninitialized" }`)

const { svgId } = defineProps({

    svgId: {
        type: String,
        required: true,
    },
})

// "mutex" that isn't actually thread-safe, but good enough for single-threaded
// javascript
let renderLocked = false
const renderQueue = []

async function renderDiagram(source) {

    // check "mutex"
    if (renderLocked) {

        // defer this invocation until after all previous invocations have
        // completed
        renderQueue.push(() => renderDiagram(source))
        return
    }

    try {

        // await mermaid.render() with the critical section locked
        renderLocked = true
        const { svg, bindFunctions } = await mermaid.render(svgId, source)
        diagramElement.value.innerHTML = svg
        bindFunctions?.(diagramElement.value)

    } finally {

        // unlock the "mutex" when exiting the critical section
        renderLocked = false
    }

    // invoke the next deferred invocation, if any
    if (renderQueue.length > 0) {

        renderQueue.shift()()
    }
}

function renderModel() {

    renderDiagram(diagramSource.value)
}

onMounted(renderModel)
</script>