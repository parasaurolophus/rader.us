<script setup>
import DefaultTheme from 'vitepress/theme'
import CopyrightComponent from './CopyrightComponent.vue'

const { Layout } = DefaultTheme
</script>

<template>
  <Layout>
    <template #doc-top>
        <CopyrightComponent />
    </template>
    <template #page-top>
        <CopyrightComponent />
    </template>
    <template #layout-bottom>
        <CopyrightComponent />
    </template>
  </Layout>
</template>
