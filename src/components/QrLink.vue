<!-- Copyright (c) 2023-2026 Kirk Rader -->

<template>
    <a :href="url" class="container" target="_blank">
        <QrComponent v-model="url" :size="size" />
        <span>
            {{ url }}
        </span>
    </a>
</template>

<style scoped>
.container {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
    justify-content: start;
    margin-bottom: 1rex;
}

.container>*:not(:first-child) {
    margin: 0 1rex;
}

@media (width >=1200px) {

    .container {
        flex-flow: column nowrap;
    }

    .container>*:not(:first-child) {
        margin: 1rem 0;
    }
}
</style>

<script setup>
import QrComponent from '@/components/QrComponent.vue'

const { size } = defineProps({

    size: {
        type: String,
        default: 'medium',
    },
})

const url = defineModel()
</script>