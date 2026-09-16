<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div class="container">

        <span>
            © Kirk Rader 2023-2026
        </span>

        <span id="omega">
            <OmegaFormula />
        </span>

    </div>

</template>

<style scoped>
.container {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
    justify-content: space-around;
    height: 100%;
}

#omega {
    display: none;
}

@media (width >=1200px) {

    #omega {
        display: inline-block;
    }
}
</style>

<script setup>
import OmegaFormula from '@/components/logic/OmegaFormula.vue'
</script>