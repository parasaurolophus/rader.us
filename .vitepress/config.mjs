import { withMermaid } from "vitepress-plugin-mermaid"
import MarkdownItCollapsible from "markdown-it-collapsible"
import MarkdownItGraphvizExec from "markdown-it-graphviz-exec"

export default withMermaid({

    srcDir: './src',

    srcExclude: [
        '**/README.md',
     ],

    title: "Kirk Rader",
    titleTemplate: ":title | Kirk Rader",
    description: "Repository of random stuff",

    head: [
        ['link', { rel: 'icon', type: 'image/png', href: '/favicon.png' }],
    ],

    themeConfig: {

        // https://vitepress.dev/reference/default-theme-config

        appearance: 'dark',

        socialLinks: [
            { icon: 'github', link: 'https://github.com/parasaurolophus' },
            {
                icon: {
                    svg: '<svg role="img" viewBox="0 0 512 512" xmlns="http://www.w3.org/2000/svg"><path d="M286.197 189.663L109 69.4539V391.791L286.197 512V189.663Z" fill="gray"/><path d="M226.983 84.1461V0L403.289 121.099V443.882L338.732 399.805V162.059L226.983 84.1461Z" fill="gray"/></svg>',
                },
                link: 'https://hyperfollow.com/kirkrader'
            },
        ],


        logo: '/kirk.png',

        docFooter: {
            prev: false,
            next: false,
        },

        nav: [
            { text: 'Home', link: '/' },
            { text: 'Music', link: '/music/' },
            { text: 'About', link: '/about' },
        ],

        sidebar: [
            {
                text: 'Here',
                items: [
                    {
                        text: 'Music',
                        link: '/music/',
                        items: [
                            { text: 'Musicography', link: '/music/musicography/' },
                            { text: 'Algorithmic Musical Composition', link: '/music/algorithmic_musical_composition/' },
                            { text: 'Ratcheting', link: '/music/ratcheting/' },
                            { text: 'For Dennis', link: '/music/for_dennis/' },
                        ]
                    },
                    { text: 'About', link: '/about' },
                    { text: 'Test Plugins', link: '/test-plugins.md'},
                ],
            },
            {
                text: 'Elsewhere',
                items: [
                    { text: 'GitHub', link: 'https://github.com/parasaurolophus' },
                    { text: 'HyperFollow', link: 'https://www.linkedin.com/in/kirkrader/' },
                ],
            },
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
