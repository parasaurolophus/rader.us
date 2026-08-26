<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <fieldset class="music-links">
        <legend>{{ externalLink.title }}</legend>
        <div>
            <QrComponent v-model="externalLink.url" size="large" />
            <a :href="externalLink.url" target="_blank">
                profile
            </a>
        </div>
    </fieldset>
</template>

<style scoped>
.music-links {
    display: inline-block;
    padding: 1vh 1vw;
    margin: 1vh 1vw;
    width: max-content;
}

.music-links>div {
    display: flex;
    flex-flow: column nowrap;
    align-items: center;
    justify-content: center;
}
</style>

<script setup>
import DetailsComponent from '@/components/DetailsComponent.vue'
import QrComponent from '@/components/QrComponent.vue'

const externalLink = defineModel()
</script>