# rader.us

mdbook sources for a personal web page

## Install

| Package        | Instructions                                                 |
|----------------|--------------------------------------------------------------|
| Rust           | <https://www.rust-lang.org/tools/install>                    |
| mdbook         | <https://rust-lang.github.io/mdBook/guide/installation.html> |
| mdbook-toc     | <https://lib.rs/crates/mdbook-toc>                           |
| mdbook-mermaid | <https://lib.rs/crates/mdbook-mermaid>                       |

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install mdbook
cargo install mdbook-toc
cargo install mdbook-mermaid
```

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

* `./book.toml` is already configured for `mdbook-mermaid`, but for new
  projects you must follow the instructions at
  <https://lib.rs/crates/mdbook-mermaid> to run
  
  ```bash
  mdbook-mermaid install path/to/book
  ```

* Use `./update_mermaid` to upgrade to the latest version of mermaid
