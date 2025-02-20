<script setup>
import DefaultTheme from 'vitepress/theme'
import CopyrightComponent from './CopyrightComponent.vue'

const { Layout } = DefaultTheme
</script>

<template>
  <Layout>
    <template #doc-bottom>
        <CopyrightComponent />
    </template>
    <template #page-bottom>
        <CopyrightComponent />
    </template>
    <template #home-features-after>
        <div style="margin-top: 1rem;">
            <CopyrightComponent />
        </div>
    </template>
  </Layout>
</template>
