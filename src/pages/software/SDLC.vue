<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <MermaidDiagram svg-id="sdlc">
        <pre>
---
title: Software Development at Enterprise Scale
---
flowchart TB

    users[Users]
    support[Customer Support]
    inception[Business Analysis,<br>Marketing]
    architecture[Architecture,<br>Design]
    development[Software Engineering]
    test[Test Engineering]
    operations[DevOps]

    users -- request features<br>report issues --> support
    support -- request features --> inception
    support -- report issues --> development
    inception -- requirements --> architecture
    inception <-- research --> users
    architecture -- analysis --> inception
    architecture -- specifications,<br>designs --> development
    development -- analysis --> architecture
    development -- release candidate --> test
    test -- report issues --> development
    test -- request features --> inception
    test -- release --> operations
    operations -- report issues --> test
    operations -- request features --> inception
        </pre>
    </MermaidDiagram>

</template>

<script setup>
import MermaidDiagram from '@/components/MermaidDiagram.vue'
</script>