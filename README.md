# rader.us

```bash
# create the project directory
mkdir vitepress-test
cd vitepress-test

# initialize a node project
npm init

# add vitepress as a dev dependency
npm add -D vitepress

# initialize vitepress
npx vitepress init

# add create .gitignore
cat << EOF > .gitignore
node_modules
.vitepress/cache
.vitepress/dist
EOF

# add mathjax support
# see
# https://vitepress.dev/guide/markdown#math-equations
# for instructions on modifying .vitepress/config.mjs
npm add -D markdown-it-mathjax3

# add mermaid extension, see
# https://emersonbottero.github.io/vitepress-plugin-mermaid/guide/getting-started.html
# for instructions on modifying .vitepress/config.mjs
npm i vitepress-plugin-mermaid mermaid -D

# run dev server
npm run docs:dev
```

* See <https://vitepress.dev/reference/default-theme-config> for the default
  theme documentation
* See <https://vitepress.dev/guide/markdown> for the list of vitepress markdown
  extensions
* See <https://vitepress.dev/reference/runtime-api> for the vitepress API
