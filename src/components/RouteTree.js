// Copyright(c) Kirk Rader 2026

import { h } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'

export default {

    props: ['root', 'links'],

    setup(props) {

        const route = useRoute()
        const router = useRouter()
        const root = props.root ?? route
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

            let routeItems = buildRouteItems(route)
            const additionalItems = links.map(link => h('li', {}, buildAnchor(link.url, link.title)))

            if (route.name === 'home') {

                const routeLink = buildRouterLink(route)

                routeItems = [h('li', {}, () => [
                    routeLink,
                    h('ul', {}, routeItems),
                ])]
            }

            return h('ul', {}, () => additionalItems.concat(routeItems))
        }

        return () => buildRouteTree(root, ...links)
    }
}