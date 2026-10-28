<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>
    <a href="https://m2vp.org" target="_blank" class="logo">
        <QrCode v-model="vcard" size="large" />
    </a>
    <div class="m2vp-logo">
        <div>
            M²VP
        </div>
        <hr>
        <div>
            Monona•McFarland Voters for Progress
        </div>
    </div>
</template>

<style scoped>
a {
    margin-bottom: 1rlh;
}
</style>

<script setup>
import QrCode from '../../components/QrCode.vue'
import { ref } from 'vue'

const vcard = ref(`BEGIN:VCARD
VERSION:4.0
KIND:org
N;CHARSET=UTF-8:M²VP
ORG;CHARSET=UTF-8:Monona●McFarland Voters for Progress
TEL:+1-608-285-2337
EMAIL:info@m2vp.org
URL:https://m2vp.org
END:VCARD`)
</script>