<!-- Copyright (c) Kirk Rader -->

<template>
    <div class="sidebar">
        <video autoplay loop muted>
            <source src="/logo256.webm" />
        </video>
        <RouteTree :root="root" :links="links" />
    </div>
</template>

<style scoped>
.sidebar {
    display: flex;
    flex-flow: column wrap;
    align-items: center;
}
</style>

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