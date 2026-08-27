<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <MermaidDiagram svg-id="home">
        <pre>
---
title: "Software Development at Scale"
---
flowchart LR

    users([Users])
    support[Support]
    inception[Marketing,<br>Business<br>Analysis]
    design[Architecture,<br>Design]
    implementation[Software<br>Development]
    testing[Quality<br>Assurance]
    operation[Operations]

    users -- report issues<br>suggest changes --> support
    support -- analysis --> inception
    inception -- requirements --> design
    design -- analysis --> inception
    design -- specifications --> implementation
    implementation -- analysis --> design
    implementation -- release<br>candidate --> testing
    testing -- report<br>issues --> implementation
    testing -- suggest<br>changes --> inception
    testing -- release --> operation
    operation -- report<br>issues --> testing
    operation -- suggest<br>changes --> inception
    support -- report<br>issues --> operation
        </pre>
    </MermaidDiagram>
</template>

<script setup>
import MermaidDiagram from '@/components/MermaidDiagram.vue'
</script>