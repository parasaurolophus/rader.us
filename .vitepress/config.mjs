import { withMermaid } from "vitepress-plugin-mermaid";

export default withMermaid({
  title: "Kirk Rader",
  description: "Repository of random stuff",

  head: [
    ['link', { rel: 'icon', type: 'image/png', href: '/favicon.png' }],
  ],

themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    logo: '/kirk.png',
    appearance: 'dark',

    nav: [
      { text: 'Home', link: '/' },
      { text: 'Test Plugins', link: '/test-plugins' },
      { text: 'README', link: '/README' },
    ],

    sidebar: [
      {
        text: 'Examples',
        items: [
          { text: 'Test Plugins', link: '/test-plugins' },
          { text: 'README', link: '/README' },
        ]
      },
    ],

    // socialLinks: [
    //   { icon: 'github', link: 'https://github.com/vuejs/vitepress' },
    // ],
  },

  // optionally, you can pass MermaidConfig
  // mermaid: {
  //    refer https://mermaid.js.org/config/setup/modules/mermaidAPI.html#mermaidapi-configuration-defaults for options
  // },

  // optionally set additional config for plugin itself with MermaidPluginConfig
  // mermaidPlugin: {
  //   class: "mermaid my-class", // set additional css classes for parent container 
  // },

  markdown: {
    math: true,
  },
})
