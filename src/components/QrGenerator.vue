<!-- Copyright (c) 2026 Kirk Rader -->

<template>
    <div>
        <textarea v-model="text" cols="40" rows="9" spellcheck="false"></textarea>
        <div ref="svg" class="svg"></div>
    </div>
</template>

<style scoped>
.svg {
    width: 128px;
    height: 128px;
    margin-top: 1rex;
}
</style>

<script setup>
import { onMounted, ref, useTemplateRef, watch } from 'vue'
import { encodeQR } from 'qr'

const svg = useTemplateRef('svg')

const text = ref(`BEGIN:VCARD
VERSION:4.0
KIND:individual
N:;parasaurolophus;;;
EMAIL:info@parasaurolophus.art
URL:https://www.parasaurolophus.art
URL:https://github.com/parasaurolophus
URL:https://hyperfollow.com/kirkrader
END:VCARD`)

function updateSvg() {

    svg.value.innerHTML = encodeQR(text.value, 'svg')
}

onMounted(updateSvg)
watch(text, updateSvg)
</script>