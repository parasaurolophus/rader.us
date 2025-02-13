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
        ],


        logo: '/kirk.png',

        nav: [
            { text: 'Home', link: '/' },
            { text: 'Music', link: '/music/' },
            { text: 'Gallery', link: '/gallery/' },
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
            {
                text: 'Gallery',
                link: '/gallery/',
                items: [
                    {
                        items: [
                            { text: 'Project 2025', link: '/gallery/Project2025.md', },
                            { text: 'Blank Verse', link: '/gallery/BlankVerse.md', },
                            { text: 'Tympanic Nerve', link: '/gallery/TympanicNerve.md', },
                            { text: 'The Auction', link: '/gallery/Auction.md', },
                        ],
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
