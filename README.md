# rader.us

```bash
# create the project directory
mkdir rader.us
cd rader.us

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
.~*
*~
dist.zip
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

# add dependency on CSV parser used by music.data.js
npm i csv-parse

# run dev server
npm run docs:dev
```

* See <https://vitepress.dev/reference/default-theme-config> for the default
  theme documentation
* See <https://vitepress.dev/guide/markdown> for the list of vitepress markdown
  extensions
* See <https://vitepress.dev/reference/runtime-api> for the vitepress API

## bobsvg sources

### Euclid's Fifth Axiom (Parallel Postulate)

```bobsvg

 A *                    * B
    \                  /
     \                /
 -----\--------------/--
       \ a )    ( b /
        \<'      `>/ +------------------------+ 
         \        /  | Parallel Postulate:    |
          \      /   |                        |
           \    /    | Lines AC and BD always | 
            \  /     | intersect when angles  | 
             \/      | a and b are both less  | 
             /\      | than 90°        {note} | 
            /  \     +------------------------+ 
         D *    * C

# Legend:
note = {
  fill: silver;
  stroke: silver;
}
```

### Playfair's Theorem

```bobsvg

         * A   / +--------------------------+ 
        /     /  | Playfair's Theorem:      | 
       /     /   | {playfair}               | 
      /     /    | For any P not on AB,     | 
     /   P *     | exactly one line can     | 
    /     /      | be drawn through P which | 
   /     /       | does not intersect AB    | 
  * B   /        +--------------------------+ 

# Legend:
playfair = {
  stroke: silver;
  fill: silver;
}
```

```bobsvg

 +---------------------------------+ 
 |       Injective Mapping         | 
 |                                 | 
 |     ______           ______     | 
 |   ,'      `.       ,'      `.   | 
 |  /  a ----- \  -  / --> e    \  | 
 | |     b ---- | - | --> f      | | 
 | |            |   |      g     | | 
 |  \          /     \          /  | 
 |   `.______.'       `.______.'   | 
 |                                 | 
 |    Domain             Range     | 
 +---------------------------------+ 

 +---------------------------------+ 
 |      Surjective Mapping         | 
 |                                 | 
 |     ______           ______     | 
 |   ,'      `.       ,'      `.   | 
 |  /  a ----- \  -  / --> e    \  | 
 | |     b ---- | - | --> f      | | 
 | |   c        |   |            | | 
 |  \          /     \          /  | 
 |   `.______.'       `.______.'   | 
 |                                 | 
 |    Domain             Range     | 
 +---------------------------------+ 

 +---------------------------------+ 
 |       Bijective Mapping         | 
 |                                 | 
 |     ______           ______     | 
 |   ,'      `.       ,'      `.   | 
 |  /  a <---- \  -  / --> e    \  | 
 | |     b <--- | - | --> f      | | 
 | |   c <----- | - | ---> g     | | 
 |  \          /     \          /  | 
 |   `.______.'       `.______.'   | 
 |                                 | 
 |    Domain             Range     | 
 +---------------------------------+ 


```
