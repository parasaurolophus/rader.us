<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>
    <div class="container">
        <span>M&sup2;VP</span>
        <QrComponent v-model="vcard" size="large" />
    </div>
</template>

<style scoped>
.container {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
    justify-content: start;
}

@media (width >=1200px) {
    .container {
        flex-flow: column nowrap;
    }
}
</style>

<script setup>
import QrComponent from '@/components/QrComponent.vue'
import { ref } from 'vue'

const vcard = ref(`BEGIN:VCARD
VERSION:4.0
KIND:organization
ORG:M2VP
EMAIL;TYPE=work:info@m2vp.org
URL:https://m2vp.org
END:VCARD`)
</script>