// Copyright (c) Kirk Rader 2026

import { createRouterMatcher, createRouter, createWebHashHistory } from 'vue-router'

const routes = [

    {
        path: '/',
        name: 'home',
        component: () => import('./pages/index.vue'),
        meta: {
            title: 'Home',
        },
    },

    {
        path: '/logic',
        name: 'logic',
        component: () => import('./pages/logic/index.vue'),
        meta: {
            title: 'Logic',
        },
    },

    // {
    //     path: '/logic/computability',
    //     name: 'computability',
    //     component: () => import('./pages/logic/computability/index.vue'),
    //     meta: {
    //         title: 'Computability',
    //     },
    // },

    {
        path: '/logic/liar',
        name: 'liar',
        component: () => import('./pages/logic/liar/index.vue'),
        meta: {
            title: 'Liar Paradox',
        },
    },

    {
        path: '/music',
        name: 'music',
        component: () => import('./pages/music/index.vue'),
        meta: {
            title: 'Music',
        },
    },

    {
        path: '/music/algorithmic',
        name: 'algorithmic',
        component: () => import('./pages/music/algorithmic/index.vue'),
        meta: {
            title: 'Algorithmic Music',
        },
    },

    {
        path: '/music/dennis',
        name: 'dennis',
        component: () => import('./pages/music/dennis/index.vue'),
        meta: {
            title: 'For Dennis',
        },
    },

    {
        path: '/music/musicography',
        name: 'musicography',
        component: () => import('./pages/music/musicography/index.vue'),
        meta: {
            title: 'Musicography',
        },
    },

    {
        path: '/music/ratcheting',
        name: 'Ratcheting',
        component: () => import('./pages/music/ratcheting/index.vue'),
        meta: {
            title: 'Ratcheting',
        },
    },
]

export const router = createRouter({
    history: createWebHashHistory(),
    routes,
})