<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="container">
        <div>
            <div class="title">
                Kirk Rader
            </div>
            <BreadCrumbs />
        </div>
        <a id="hyperfollowqr" :href="externalLinks.hyperFollow" target="_blank">
            <QrComponent v-model="externalLinks.hyperFollow" />
        </a>
    </div>
</template>

<style scoped>
.container {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
}

.title {
    font-size: x-large;
}

#hyperfollowqr {
    margin-left: auto;
}
</style>

<script setup>
import BreadCrumbs from '@/components/BreadCrumbs.vue'
import QrComponent from '@/components/QrComponent.vue'
import { inject } from 'vue'

const externalLinks = inject('externalLinks')
</script>