<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="titlebar">
        <div>
            <div class="title">
                Kirk Rader
            </div>
            <BreadCrumbs />
        </div>
        <a id="hyperfollowqr" :href="otherLinks.hyperFollow.url" target="_blank">
            <QrComponent v-model="otherLinks.hyperFollow.url" />
        </a>
        <ToggleThemeButton />
    </div>
</template>

<style scoped>
.titlebar {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
}

.title {
    font-size: xx-large;
}

#hyperfollowqr {
    margin-left: auto;
    margin-right: 1em;
}
</style>

<script setup>
import BreadCrumbs from '@/components/BreadCrumbs.vue'
import QrComponent from '@/components/QrComponent.vue'
import ToggleThemeButton from '@/components//ToggleThemeButton.vue'
import { inject } from 'vue'

const otherLinks = inject('otherLinks')
</script>