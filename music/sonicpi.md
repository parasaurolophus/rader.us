---
outline: false
---

<script setup>
import { data } from '/all.data.js'
</script>

# SonicPi "Scores"

<template v-for="(score) in data">
  <template v-if="score.file.endsWith('.rb')">
    <h4>{{ /[^/]*$/.exec(score.file)[0] }}</h4>
    <pre>{{ score.contents }}</pre>
  </template>
</template>
