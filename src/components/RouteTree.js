// Copyright(c) Kirk Rader 2026

import { h, inject } from 'vue'
import { RouterLink, useRouter } from 'vue-router'

export default {

    props: {

        root: {
            required: false,
        },
    },

    setup(props) {

        const externalLinks = inject('externalLinks')
        const router = useRouter()
        const root = props.root ?? router.getRoutes().filter(route => route.path === '/' || route.name === 'home')[0]

        function buildAnchor(url, title) {

            return h('a', { href: url, target: '_blank' }, () => [title])
        }

        function buildRouteBody(route) {

            return [
                buildRouterLink(route),
                buildRouteTree(route)
            ]
        }

        function buildRouteItems(route) {

            const re = new RegExp(`^${route.path === '/' ? '' : route.path}/[^/]+$`)
            const children = router.getRoutes().filter(r => re.test(r.path))

            children.sort((a, b) => a.path.localeCompare(b.path))

            return children.map(child => h('li', {}, () => buildRouteBody(child)))
        }

        function buildRouterLink(route) {

            return h(RouterLink, { to: route.path }, () => [route.meta?.title ?? route.name ?? route.path])
        }

        function buildRouteTree(route) {

            return h('ul', {}, () => buildRouteItems(route))
        }

        if (root.name === 'home') {

            return () => h(
                'ul',
                {},
                () => [
                    h('li', {}, () => [buildAnchor(externalLinks.value.hyperFollow, 'HyperFollow')]),
                    h('li', {}, () => [buildAnchor(externalLinks.value.github, 'GitHub')]),
                    h('li', {}, () => [
                        buildRouterLink(root),
                        buildRouteTree(root),
                    ]),
                ],
            )
        }

        return () => buildRouteTree(root)
    }
}