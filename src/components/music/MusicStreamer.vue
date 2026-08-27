<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <fieldset class="wrapper">
        <legend>{{ selectedLink.title }}</legend>
        <a :href="selectedLink.url" target="_blank">
            <QrComponent v-model="selectedLink.url" :size="size" />
        </a>
        <select v-model="selectedLink">
            <template v-for="link in externalLinks">
                <option :value="link">{{ link.title }}</option>
            </template>
        </select>
    </fieldset>
</template>

<style scoped>
.wrapper {
    display: flex;
    flex-flow: column nowrap;
    align-items: center;
    justify-content: center;
    width: max-content;
}

.wrapper>* {
    margin: 1em;
    padding: 1em;
}
</style>

<script setup>
import QrComponent from '@/components/QrComponent.vue'
import { inject, ref } from 'vue'

const { size } = defineProps({

    size: {
        type: String,
        default: 'medium',
    },
})

const externalLinks = inject('externalLinks')
const selectedLink = ref(externalLinks.value.appleMusic)
</script>