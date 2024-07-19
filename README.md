# rader.us

mdbook sources for a personal web page

## Install

* [Install Rust](https://www.rust-lang.org/tools/install)
* [Install mdbook](https://rust-lang.github.io/mdBook/guide/installation.html)
* [Install mdbook-mermaid](https://lib.rs/crates/mdbook-mermaid)

## Test

```bash
mdbook clean ; mdbook serve --open
```

## Deploy

```bash
./zipbook
```

...then unzip `./book.zip` in the web server's root directory

## Notes

* `./book.toml` is already configured for `mdbook-mermaid`, but for new
  projects you must follow the instructions at
  <https://lib.rs/crates/mdbook-mermaid> to run `mdbook-mermaid install`

* Use `./update_mermaid` to upgrade to the latest version of mermaid
