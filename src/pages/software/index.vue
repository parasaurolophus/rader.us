<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div>

        <div class="container">

            <div class="text">

                <h1>Software Development</h1>

                <p>
                    Forget trendy (or no longer so trendy) "methodologies"
                    hawked by pricey consultants. If you are developing
                    enterprise-scale software successfully, this is what you are
                    really doing.
                </p>

                <p>
                    Note that the various boxes represent functional areas of
                    concern, not necessarily distinct teams. A challenge every
                    organization faces as it grows is that these areas of
                    concern can too easily harden into silos. Successful
                    organizations encourage continous communication and
                    collaboration, as depicted in this diagram through numerous
                    bi-directional dependencies.
                </p>

            </div>

            <SDLC class="diagram" />

        </div>

    </div>

</template>

<style scoped>
@media (orientation: landscape) {

    .container {
        display: flex;
        flex-flow: row nowrap;
        justify-content: start;
    }

    .text {
        width: 50%;
        margin-right: 1rem;
    }

    .diagram {
        width: 27.5rem;
    }
}
</style>

<script setup>
import SDLC from '@/pages/software/SDLC.vue'
</script>