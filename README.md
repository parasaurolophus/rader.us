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

```bash
mdbook clean ; mdbook serve --open
```

## Build

```bash
mdbook clean ; mdbook build
```

HTML output will be in the _book_ subdirectory.

## Deploy

Upload the contents of the _book_ subdirectory to the server's root directory.

```bash
./zipbook
```

As a convenience, the _zipbook_ bash script will clean, build and then create a
zip archive of the _book_ directory named _book.zip_ which can then be
unzippped in the web server's root directory

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

* Use `./update_mermaid` to upgrade to the latest version of mermaid
