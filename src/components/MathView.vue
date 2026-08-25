<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <component ref="math" :is="toplevel ? 'math' : 'mrow'">
        <slot></slot>
    </component>
</template>

<script setup>
import { onMounted, ref, useTemplateRef } from 'vue'

const math = useTemplateRef('math')

const toplevel = ref(true)

function isToplevel() {

    const elements = document.getElementsByTagName('math')

    for (let index = 0; index < elements.length; ++index) {

        const element = elements.item(index)

        if (element === math.value) {

            continue
        }

        if (element.contains(math.value)) {

            toplevel.value = false
            return
        }
    }

    toplevel.value = true
}

onMounted(isToplevel)
</script>