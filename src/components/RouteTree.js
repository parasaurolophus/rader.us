// Copyright(c) Kirk Rader 2026

import { h, inject } from 'vue'
import { RouterLink, useRouter } from 'vue-router'

export default {

    props: ['root', 'links'],

    setup(props) {

        const externalLinks = inject('externalLinks')
        const router = useRouter()
        const root = props.root ?? router.getRoutes().filter(route => route.path === '/' || route.name === 'home')[0]
        const links = props.links ?? []

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

        function buildRouteTree(route, ...links) {

            const body = buildRouteItems(route)

            for (let link of links) {

                body.unshift(h('li', {}, buildAnchor(link.url, link.title)))
            }

            return h('ul', {}, () => body)
        }

        if (root.name === 'home') {

            return () => h(
                'ul',
                {},
                () => [
                    h('li', {}, () => [buildAnchor(externalLinks.value.hyperFollow.url, externalLinks.value.hyperFollow.title)]),
                    h('li', {}, () => [buildAnchor(externalLinks.value.github.url, externalLinks.value.github.title)]),
                    h('li', {}, () => [
                        buildRouterLink(root),
                        buildRouteTree(root),
                    ]),
                ],
            )
        }

        return () => buildRouteTree(root, ...links)
    }
}