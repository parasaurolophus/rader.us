<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="container">
        <template v-for="(destination, index) of path">
            <MdiIcon v-if="index > 0" :path="mdiPlay" class="separator" />
            <span v-if="destination.to === null" class="label">{{ destination.title }}</span>
            <RouterLink v-else :to="destination.to">{{ destination.title }}</RouterLink>
        </template>
    </div>
</template>

<style scoped>
.container {
    display: flex;
    align-items: center;
}

.separator {
    margin: 0 0.25em;
}

.label {
    font-weight: bolder;
}
</style>

<script setup>
import { computed } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { mdiPlay } from '@mdi/js'
import MdiIcon from '@/components/MdiIcon.vue'

const path = computed(computePath)
const route = useRoute()
const router = useRouter()

function computePath() {

    let uri = route.path.split('/')

    if (uri[0] === '') {

        uri[0] = 'home'
    }

    if (uri[uri.length - 1] === '') {

        uri.pop()
    }

    if (uri.length === 1) {

        return [{ title: 'home', to: null }]
    }

    return uri.map((s, i, u) => {

        const url = toUrl(i, u)
        const r = router.resolve(url)
        const title = r?.meta?.title ?? r?.name ?? s

        return {
            title: title,
            to: url,
        }
    })
}

function toUrl(index, uri) {

    if (index === 0) {

        return { name: 'home' }
    }

    if (index === uri.length - 1) {

        return null
    }

    return uri.slice(1, index + 1).reduce((a, s, i, u) => a.concat(s, i < u.length - 1 ? '/' : ''), '/')
}
</script>