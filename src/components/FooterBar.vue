<!-- Copyright (c) Kirk Rader 2026 -->

<template>

    <div class="footer">

        <span>
            © Kirk Rader 2023-2026
        </span>

        <div id="omega">
            <OmegaFormula />
        </div>

    </div>

</template>

<style scoped>
.footer {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
    justify-content: space-around;
}

#omega {
    display: none;
}

@media (width >= 1200px) {

    #omega {
        display: block;
    }
}
</style>

<script setup>
import OmegaFormula from '@/components/logic/OmegaFormula.vue'
</script>