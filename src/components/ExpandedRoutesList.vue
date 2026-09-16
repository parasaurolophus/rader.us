<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <RouteTree :root="root" :links="links" />
</template>

<script setup>
import RouteTree from '@/components/RouteTree'
import { useRouter } from 'vue-router'
import { computed, inject, onMounted, ref, watch } from 'vue'

const otherLinks = inject('otherLinks')
const softwareLinks = inject('softwareLinks')
const router = useRouter()
const root = computed(() => router.resolve({ name: 'home' }))
const links = ref([])

function updateLinks() {

    links.value.splice(0)

    if (otherLinks.value.hyperFollow) {

        links.value.push(otherLinks.value.hyperFollow)
    }

    if (softwareLinks.value.github) {

        links.value.push(softwareLinks.value.github)
    }
}

onMounted(updateLinks)
watch(otherLinks, updateLinks)
watch(softwareLinks, updateLinks)
</script>