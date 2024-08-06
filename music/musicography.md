<script setup>
import { data } from '/music.data.js'
</script>

# Musicography

> - [Amazon Music](amazon.md)
> - [Apple Music](apple.md)
> - [Bandcamp](bandcamp.md)
> - [Spotify](spotify.md)
> - [Youtube Music](youtube.md)
>
> _Or search for "Kirk Rader" on the music service you prefer_

## List of Published Works

<template v-for="(album) in data">
    <h3>{{ album[0].album }}</h3>
    <p>UPC {{ album[0].upc }}</p>
    <table>
        <tr>
            <th>Track</th>
            <th>Title</th>
            <th>ISRC</th>
        </tr>
        <tr v-for="(item) in album">
            <td>{{ item.track }}</td>
            <td>{{ item.title }}</td>
            <td>{{ item.isrc }}</td>
        </tr>
    </table>
</template>

## What Does the Title of My First Album, _Undecidable_, mean?

Even though you didn't ask and probably weren't wondering, the title
_Undecidable_ and its cover art featuring a formula of the [lambda
calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is an inside joke with
myself in reference to my studying formal linguistics and symbolic logic at
UCLA &mdash; including taking classes from [Alonzo
Church](https://en.wikipedia.org/wiki/Alonzo_Church), himself &mdash; during
the period when many of those tracks were recorded.

Given:

$$
\begin{align*}
    \text{Let } \Omega & = \omega \ \omega \\
    \text{where } \omega & = \lambda x.x \ x
\end{align*}
$$

The value of $\Omega$ is "undecidable" according to the rules of the lambda
calculus because the calculation of $\lambda x.x \hskip0.25em x$ never
terminates when applied to itself. I experimented a lot with feedback from the
signal path back into the control path (the sort of thing you can really only
do with modular analog synthesizers) so I adopted this self-referential
mathematical formula as a logo. This resonated (pun intended) with me because
undecidability due to self-reference is the key not only to the ancient [liar
paradox](https://en.wikipedia.org/wiki/Liar_paradox), it is also the basis of
Goedel's [incompleteness
proof](https://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems) in
formal linguistics, the explanation of why there is no general solution to the
[halting problem](https://en.wikipedia.org/wiki/Halting_problem) in computer
science and underlies many other similarly consequential results in math and
science, while feedback (a physical manifestation of "self reference") is a way
to produce highly complex (and often catastrophic) results from simple
mechanical or electronic systems.

As an even less relevant aside, I chose $\Omega$ (the last letter of the
Greek alphabet) as the name of the undecidable result because it is easier to
render using computer keyboards than $\aleph_{0}$ that is the name
traditionally used when referring to the cardinality of the set of natural
numbers and that, according to accepted wisdom, is the smallest infinite
magnitude. It also happens to be the "stack depth" of an infinitely recursive
function. Why is that amusing? Well, I was also teaching myself computer
programming later in the same period that I was creating those tracks, so...
maybe it isn't all that amusing. But it makes me smile to think that the most
abstruse branches of "pure" mathematics like [computability
theory](https://en.wikipedia.org/wiki/Computability_theory) can inadvertently
produce literally world-changing effects such as the post-industrial
"Information Age." According to common terminology, $\Omega$ more properly
should refer to $\aleph_{1}$ rather than $\aleph_{0}$, the former being
the cardinality of the set of all countable numbers. Since the cardinality of a
set's power set must be larger than that set's own cardinality, $\aleph_{0} <
\aleph_{1}$ so $\aleph_{0} \neq \Omega$. So if I'd had access to better
graphical tools at the time I would have used this as the logo:

$$
\begin{align*}
    \text{Let } \aleph_{0} & = \omega_{0} \ \omega_{0} \\
    \text{where } \omega_{0} & = \lambda x.x \ x
\end{align*}
$$

If I ever publish a re-issue or remixes of any of the tracks on _Undecidable_,
you will already be in on the joke of the logo I plan to use.
