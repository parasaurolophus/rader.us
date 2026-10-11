<!-- Copyright (c) Kirk Rader -->

<template>
    <div class="container">
        <details ref="details" class="hide-border">
            <summary>
                <MdiIcon :path="mdiMenu" />
            </summary>
            <ExpandedRoutesList />
        </details>
    </div>
</template>

<style scoped>
.container {
    width: max-content;
    padding-right: 1rem;
    margin-right: 1rem;
    background-color: var(--highlight);
}

.hide-border {
    border-style: none;
}

summary {
    display: inline-block;
}
</style>

<script setup>
import ExpandedRoutesList from '@/components/ExpandedRoutesList.vue'
import MdiIcon from '@/components/MdiIcon.vue'
import { inject, onMounted, useTemplateRef, watch } from 'vue'
import { useRouter } from 'vue-router'
import { mdiMenu } from '@mdi/js'

const details = useTemplateRef('details')
const router = useRouter()
const isLargeScreen = inject('isLargeScreen')

function updateDetails() {

    details.value.open = isLargeScreen || router.currentRoute.value.name === 'home'
}

onMounted(() => router.afterEach(updateDetails))
watch(isLargeScreen, updateDetails)
</script>