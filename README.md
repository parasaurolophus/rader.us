# rader.us

mdbook sources for a personal web page

## Install

| Package         | Instructions                               |
|-----------------|--------------------------------------------|
| Rust            | <https://www.rust-lang.org/tools/install>  |
| graphviz        | <https://graphviz.org/>                    |
| mdbook          | <https://crates.io/crates/mdbook>          |
| mdbook-toc      | <https://crates.io/crates/mdbook-toc>      |
| mdbook-mermaid  | <https://crates.io/crates/mdbook-mermaid>  |
| mdbook-graphviz | <https://crates.io/crates/mdbook-graphviz> |

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
sudo apt-get update ; sudo apt-get install graphviz graphviz-doc
cargo install mdbook
cargo install mdbook-toc
cargo install mdbook-mermaid
cargo install mdbook-graphviz
```

See [Notes](#notes) for additional configuration information.

## Test

### Test Manual

This will open your default browser

```bash
mdbook clean ; mdbook serve --open
```

### Test Using Makefile

```bash
make clean test
```

## Build

HTML output will be in the _book_ subdirectory.

### Build Manually

```bash
mdbook clean ; mdbook build
```

### Build Using Makefile

```bash
make clean build
```

## Deploy

After [testing](#test) and [building](#build), upload the contents of the
_book_ subdirectory to the server's root directory. As a convenience, you can
create a zip archive of the deployment directory using:

```bash
# just archive the current contents of the build output directory
make zip

# create an archive after completely cleaning and rebuilding:
make all
```

## Update _mermaid.min.js_

To update to the latest version of Mermaid:

```bash
make mermaid
```

(But beware of breaking changes to Mermaid diagram or initialization script
syntax.)

## Notes

* [book.toml](./book.toml) is already configured for `mdbook-toc`,
  `mdbook-mermaid` and `mdbook-graphviz` but for new projects you must follow
  the instructions at <https://lib.rs/crates/mdbook-mermaid> to run
  
  ```bash
  mdbook-mermaid install path/to/book
  ```

  as well as manually adding the required configuration to the
  `[preproccessor]` section, resulting in

  ```toml
  [preprocessor.toc]
  command = "mdbook-toc"
  renderer = ["html"]

  [preprocessor.mermaid]
  command = "mdbook-mermaid"

  [preprocessor.graphviz]
  command = "mdbook-graphviz"
  output-to-file = false # defaults to false, change to true to create SVG files instead of rendering them inline
  ```
