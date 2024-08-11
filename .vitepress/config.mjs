import { withMermaid } from "vitepress-plugin-mermaid"
import MarkdownItCollapsible from "markdown-it-collapsible"
import MarkdownItGraphvizExec from "markdown-it-graphviz-exec"

export default withMermaid({
  title: "Kirk Rader",
  description: "Repository of random stuff",

  head: [
    ['link', { rel: 'icon', type: 'image/png', href: '/favicon.png' }],
  ],

  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    appearance: 'dark',

    footer: {
      message: '',
      copyright: 'Copyright 2024 <a href="https://github.com/parasaurolophus">Kirk Rader</a>'
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/parasaurolophus' },
      { icon: 'linkedin', link: 'https://www.linkedin.com/in/kirkrader/' },
    ],


    logo: '/kirk.png',

    nav: [
      { text: 'Home', link: '/' },
      { text: 'Music', link: '/music/music' },
      { text: 'Philosopy', link: '/philosophy/computability' },
      { text: 'About', link: '/about' },
    ],

    sidebar: [
      {
        text: 'Music', link: '/music/music',
        items: [
          { text: 'SonicPi "Scores"', link: '/music/sonicpi' },
          { text: 'MP3', link: '/music/mp3' },
        ]
      },
      {
        text: 'Philosophy',
        items: [
          {
            text: 'Computability', link: '/philosophy/computability'
          }
        ],
      },
      { text: 'Test Plugins', link: '/test-plugins' },
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
