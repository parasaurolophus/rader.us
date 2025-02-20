import { withMermaid } from "vitepress-plugin-mermaid"
import MarkdownItCollapsible from "markdown-it-collapsible"
import MarkdownItGraphvizExec from "markdown-it-graphviz-exec"

export default withMermaid({
    title: "Home",
    titleTemplate: "Kirk Rader",
    description: "Repository of random stuff",

    head: [
        ['link', { rel: 'icon', type: 'image/png', href: '/favicon.png' }],
    ],

    themeConfig: {
        // https://vitepress.dev/reference/default-theme-config
        appearance: 'dark',

        socialLinks: [
            { icon: 'github', link: 'https://github.com/parasaurolophus' },
            { icon: 'linkedin', link: 'https://www.linkedin.com/in/kirkrader/' },
            {
                icon: {
                    svg: '<svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><title>HyperFollow</title><rect style="fill:#AAAAAA;stroke:#AAAAAA" width="24" height="24" x="0" y="0"/></svg>',
                },
                link: 'https://hyperfollow.com/kirkrader'
            },
        ],


        logo: '/kirk.png',

        nav: [
            { text: 'Home', link: '/' },
            { text: 'Music', link: '/music/' },
            { text: 'About', link: '/about' },
        ],

        sidebar: [
            {
                text: 'Music',
                link: '/music/',
                items: [
                    {
                        items: [
                            { text: 'Musicography', link: '/music/musicography/' },
                            { text: 'Algorithmic Musical Composition', link: '/music/algorithmic_musical_composition/' },
                            { text: 'Ratcheting', link: '/music/ratcheting/' },
                            { text: 'For Dennis', link: '/music/for_dennis/' },
                        ]
                    }
                ]
            },
            { text: 'About', link: '/about' },
        ],

        outline: {
            level: 'deep',
            label: 'On this page',
        }
    },

    markdown: {
        math: true,
        config: (md) => {
            md.use(MarkdownItCollapsible)
            md.use(MarkdownItGraphvizExec)
        }
    },
})
