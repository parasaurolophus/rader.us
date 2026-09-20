&copy; Kirk Rader 2023-2026

# rader.us

[Vue] source for a vanity web site hosted at <https://www.rader.us>

See [./LICENSE](./LICENSE)

> _**Note:** Currently, only Windows is supported by the `clean` script in
> `package.json`. The `zip` script should work with any version of `tar` that
> supports zip file format based on the `.zip` file extension using the `a`
> option._

## Dependencies / Attributions

### Development Platform

- [git]
  - Developed and tested using version `2.55.0.windows.5`
- [Node]
  - Developed and tested using version `24.14.0` on Windows
- [npm]
  - Developed and test using version `11.19.1` on Windows

### Node Dependencies

- [@mdi/js]
- [mermaid]
- [qr]
- [vue-router]
- [@vitejs/plugin-vue]
- [vite]
- [vite-plugin-vue-devtools]

See `dependencies` and `devDependencies` blocks in [./package.json] for versions
in use. See [./package-lock.json] for transitive dependencies.

## Installation

```powerview
git clone git@github.com:parasaurolophus/rader.us.git
cd rader.us
npm i
```

If you are not me, either

- Fork <https://github.com/parasaurolophus/rader.us> and use your fork's git URL in `git clone`
- Use `git clone https://github.com/parasaurolophus/rader.us.git` to create a local clone

## Dev / Test

```powerview
clear; npm run clean; npm run dev
```

Use the displayed link in the console to open browser

## Build

```powerview
clear; npm run clean; npm run build
```

Output will be in `dist` subdirectory

## Deploy

```powerview
clear; npm run clean; npm run build; npm run zip
```

Upload and extract contents of `dist.zip` to web server's root directory

---

## Original Vue README.md content

### Recommended IDE Setup

[VS Code](https://code.visualstudio.com/) + [Vue (Official)](https://marketplace.visualstudio.com/items?itemName=Vue.volar) (and disable Vetur).

### Recommended Browser Setup

- Chromium-based browsers (Chrome, Edge, Brave, etc.):
  - [Vue.js devtools](https://chromewebstore.google.com/detail/vuejs-devtools/nhdogjmejiglipccpnnnanhbledajbpd)
  - [Turn on Custom Object Formatter in Chrome DevTools](http://bit.ly/object-formatters)
- Firefox:
  - [Vue.js devtools](https://addons.mozilla.org/en-US/firefox/addon/vue-js-devtools/)
  - [Turn on Custom Object Formatter in Firefox DevTools](https://fxdx.dev/firefox-devtools-custom-object-formatters/)

### Customize configuration

See [Vite Configuration Reference](https://vite.dev/config/).

### Project Setup

```sh
npm install
```

### Compile and Hot-Reload for Development

```sh
npm run dev
```

### Compile and Minify for Production

```sh
npm run build
```

[@mdi/js]: https://www.npmjs.com/package/@mdi/js
[git]: https://git-scm.com/
[mermaid]: https://www.npmjs.com/package/mermaid
[Node]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[./package.json]: ./package.json
[./package-lock.json]: ./package-lock.json
[qr]: https://www.npmjs.com/package/qr
[Vue]: https://vuejs.org/
[vue-router]: https://www.npmjs.com/package/vue-router
[@vitejs/plugin-vue]: https://www.npmjs.com/package/@vitejs/plugin-vue
[vite]: https://www.npmjs.com/package/vite
[vite-plugin-vue-devtools]: https://www.npmjs.com/package/vite-plugin-vue-devtools
