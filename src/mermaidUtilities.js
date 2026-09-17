// Copyright (c) 2026 Kirk Rader

import mermaid from 'mermaid'

// "mutex" that isn't actually thread-safe, but good enough for single-threaded
// javascript
let renderLocked = false
const renderQueue = []

async function renderDiagram(svgId, source, element) {

    // check "mutex"
    if (renderLocked) {

        // defer this invocation until after all previous invocations have
        // completed
        renderQueue.push(() => renderDiagram(svgId, source, element))
        return
    }

    try {

        // await mermaid.render() with the critical section locked
        renderLocked = true
        const { svg, bindFunctions } = await mermaid.render(svgId, source)
        element.innerHTML = svg
        bindFunctions?.(element)

    } finally {

        // unlock the "mutex" when exiting the critical section
        renderLocked = false
    }

    // invoke the next deferred invocation, if any
    if (renderQueue.length > 0) {

        renderQueue.shift()()
    }
}

export { renderDiagram }