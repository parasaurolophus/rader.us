<!-- Copyright (c) Kirk Rader 2023-2026 -->

<template>

    <div>

        <h1>Music</h1>

        <p>
            <MusicStreamer size="x-large" />
        </p>

        <p>
            <i>
                ...or search for
                "<CopyableSpan>Kirk Rader</CopyableSpan>"
                on the music service you prefer
            </i>
        </p>

    </div>

</template>

<script setup>
import CopyableSpan from '@/components/CopyableSpan.vue'
import MusicStreamer from '@/components/music/MusicStreamer.vue'
</script>