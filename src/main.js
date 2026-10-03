// Copyright (c) 2023-2026 Kirk Rader

import { createApp } from 'vue'
import App from './App.vue'
import { router } from './router.js'
import 'highlight.js/styles/stackoverflow-dark.css'
import hljs from 'highlight.js/lib/core'
import bnf from 'highlight.js/lib/languages/bnf'
import go from 'highlight.js/lib/languages/go'
import javascript from 'highlight.js/lib/languages/javascript'
import ruby from 'highlight.js/lib/languages/ruby'
import scheme from 'highlight.js/lib/languages/scheme'
import hljsVuePlugin from "@highlightjs/vue-plugin"

hljs.registerLanguage('bnf', bnf)
hljs.registerLanguage('go', go)
hljs.registerLanguage('javascript', javascript)
hljs.registerLanguage('ruby', ruby)
hljs.registerLanguage('scheme', scheme)

createApp(App)
    .use(router)
    .use(hljsVuePlugin)
    .mount('#app')
