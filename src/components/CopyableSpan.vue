<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <span @click="copyText" class="copyable">
        <slot></slot>
    </span>
    <dialog ref="copied" closedby="any">copied</dialog>
</template>

<script setup>
import { useTemplateRef } from 'vue'

const copied = useTemplateRef('copied')

function copyText(event) {

    try {

        navigator.clipboard.writeText(event.target.innerText)
        copied.value.innerText = `copied "${event.target.innerText}" to clipboard`
        copied.value.show()
        setTimeout(() => copied.value.close(), 3000)

    } catch (error) {

        copied.value.innerText = error
    }
}
</script>