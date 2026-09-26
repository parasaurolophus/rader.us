// Copyright (c) Kirk Rader 2026

import { createApp } from 'vue'
import App from './App.vue'
import { router } from './router.js'
import 'highlight.js/styles/stackoverflow-dark.css'
import hljs from 'highlight.js/lib/core'
import bnf from 'highlight.js/lib/languages/bnf'
import ruby from 'highlight.js/lib/languages/ruby'
import scheme from 'highlight.js/lib/languages/scheme'
import hljsVuePlugin from "@highlightjs/vue-plugin"

hljs.registerLanguage('bnf', bnf)
hljs.registerLanguage('ruby', ruby)
hljs.registerLanguage('scheme', scheme)

createApp(App)
    .use(router)
    .use(hljsVuePlugin)
    .mount('#app')
