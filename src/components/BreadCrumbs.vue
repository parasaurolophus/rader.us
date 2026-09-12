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
    flex-flow: row nowrap;
    align-items: center;
    justify-content: start;
}

.separator {
    margin: 0 0.25rem;
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

const route = useRoute()
const router = useRouter()

const path = computed(() => {

    let uri = route.path.split('/')

    if (uri[uri.length - 1] === '') {

        uri.pop()
    }

    if (uri.length === 1) {

        if (uri[0] === '') {

            return [{
                title: getTitle({ name: 'home' }, 'home'),
                to: null,
            }]
        }

        return [{
            title: getTitle(route.path, uri[0]),
            to: null,
        }]
    }

    return uri.map((s, i, u) => {

        const url = toUrl(i, u)

        return {
            title: getTitle(url, s),
            to: i === u.length - 1 ? null : url,
        }
    })
})

function getTitle(to, s) {

    const r = router.resolve(to === '' ? '/' : to)

    return r?.meta?.title ?? r?.name ?? s
}

function toUrl(index, uri) {

    if (index === 0) {

        return { name: 'home' }
    }

    return uri.slice(1, index + 1).reduce((a, s, i, u) => {

        return a.concat(s, i < u.length - 1 ? '/' : '')
    }, '/')
}
</script>