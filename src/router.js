// Copyright (c) Kirk Rader 2026

import { createWebHashHistory, createRouter } from 'vue-router'

const routes = [

    {
        path: '/',
        name: 'home',
        component: () => import('./pages/HomePage.vue'),
        meta: {
            title: 'Home',
        },
    },

    {
        path: '/logic',
        name: 'logic',
        component: () => import('./pages/logic/LogicPage.vue'),
        meta: {
            title: 'Logic',
        },
    },

    {
        path: '/logic/computability',
        name: 'computability',
        component: () => import('./pages/logic/ComputabilityPage.vue'),
        meta: {
            title: 'Computability',
        },
    },

    {
        path: '/logic/ieee',
        name: 'ieee',
        component: () => import('./pages/logic/Ieee754.vue'),
        meta: {
            title: 'IEEE 754',
        },
    },

    {
        path: '/logic/liar',
        name: 'liar',
        component: () => import('./pages/logic/LiarParadoxPage.vue'),
        meta: {
            title: 'Liar Paradox',
        },
    },

    {
        path: '/music',
        name: 'music',
        component: () => import('./pages/music/MusicPage.vue'),
        meta: {
            title: 'Music',
        },
    },

    {
        path: '/music/algorithmic',
        name: 'algorithmic',
        component: () => import('./pages/music/AlgorithmicMusicPage.vue'),
        meta: {
            title: 'Algorithmic Music',
        },
    },

    {
        path: '/music/dennis',
        name: 'dennis',
        component: () => import('./pages/music/ForDennisPage.vue'),
        meta: {
            title: 'For Dennis',
        },
    },

    {
        path: '/music/musicography',
        name: 'musicography',
        component: () => import('./pages/music/MusicographyPage.vue'),
        meta: {
            title: 'Musicography',
        },
    },
]

export const router = createRouter({
    history: createWebHashHistory(),
    routes,
})