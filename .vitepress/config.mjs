import { withMermaid } from "vitepress-plugin-mermaid";

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
      { text: 'Music', link: '/music' },
      { text: 'About', link: '/about' },
    ],

    sidebar: [
      { text: 'Music', link: '/music' },
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
  },
})