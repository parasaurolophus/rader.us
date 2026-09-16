<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <fieldset v-if="selectedLink" class="wrapper">
        <legend>{{ selectedLink.title }}</legend>
        <select v-model="selectedLink">
            <template v-for="link of links ?? []">
                <option :value="link">{{ link.title }}</option>
            </template>
        </select>
        <a :href="selectedLink.url" target="_blank">
            <QrComponent v-model="selectedLink.url" :size="size" />
        </a>
        <span v-if="selectedLink">
            <a :href="selectedLink.url" target="_blank">
                {{ selectedLink.url }}
            </a>
        </span>
    </fieldset>
</template>

<style scoped>
.wrapper {
    display: flex;
    flex-flow: column nowrap;
    align-items: center;
    justify-content: center;
    width: max-content;
    max-width: 100%;
}

.wrapper>* {
    margin: 1rem;
    padding: 1rem;
}
</style>

<script setup>
import QrComponent from '@/components/QrComponent.vue'
import { onMounted, ref, watch } from 'vue'

const { size } = defineProps({

    size: {
        type: String,
        default: 'medium',
    },
})

const links = defineModel()
const selectedLink = ref(links.value[0])

function update() {

    if (links.value && links.value.length > 0) {

        selectedLink.value = links.value[0]
    }
}

onMounted(update)
watch(links, update)
</script>