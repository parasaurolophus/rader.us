<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <div class="container">
        <div>
            <div class="title">
                Kirk Rader
            </div>
            <BreadCrumbs />
        </div>
        <a id="hyperfollowqr" href="https://hyperfollow.com/kirkrader" target="_blank">
            <img src="/qr-hyperfollow.png" alt="https://hyperfollow.com/kirkrader">
        </a>
    </div>
</template>

<style scoped>
.container {
    display: flex;
    flex-flow: row nowrap;
    align-items: center;
}

.title {
    font-size: x-large;
}

#hyperfollowqr {
    margin-left: auto;
}

#hyperfollowqr>img {
    width: calc(0.75 * var(--headerheight));
    height: calc(0.75 * var(--headerheight));
    border-radius: initial;
}
</style>

<script setup>
import BreadCrumbs from '@/components/BreadCrumbs.vue'
</script>