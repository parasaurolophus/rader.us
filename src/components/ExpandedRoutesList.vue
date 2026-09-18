<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <RouteTree :root="root" :links="links" />
</template>

<script setup>
import RouteTree from '@/components/RouteTree'
import { useRouter } from 'vue-router'
import { computed, inject } from 'vue'

const otherLinks = inject('otherLinks')
const router = useRouter()
const root = computed(() => router.resolve({ name: 'home' }))
const links = computed(()=>Object.values(otherLinks.value))
</script>