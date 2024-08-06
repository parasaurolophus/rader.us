<script setup>
import { data } from '/sonicpi.data.js'
</script>

# Music

> - [Amazon Music](amazon.md)
> - [Apple Music](apple.md)
> - [Bandcamp](bandcamp.md)
> - [Spotify](spotify.md)
> - [Youtube Music](youtube.md)
>
> _Or search for "Kirk Rader" on the music service you prefer_

[I started creating electronic music](./yesterday.md) on the West Coast of the
US, in the "West Coast Style," long before I knew that there was such a thing.
My musical tastes have always been outside the mainstream. The first album I
purchased with my "own" money was a vinyl recording (there were no other
commercially significant recording media for sale in the mid 1960's) of
Schubert's _Unfinished Symphony_ which I had worn the grooves out of by the
time I turned 7 years old. When I was studying formal linguistics and teaching
myself to program computers in my late teens and early twenties, I did so to a
personal soundtrack that featured Lou Reed's _Metal Machine Music_ (yes, the
album consisting of 4 LP sides of electric guitar feedback and effects pedal
noise that literally ended his career just as it was beginning to take off in
the mid 70's and then went on to become the [stuff of
legends](https://www.zeitkratzer.de/metal-metal-machine-music-zeitkratzer-performing-lou-reeds-milestone-)
decades later).

My favorite composers include Tallis, Monteverdi, Vivaldi, Bach,
Handel, Beethoven, Glass and Byrne as well as hard-core mid-century
experimentalists like Stockhausen and Subotnick. If you have never
heard any of my own stuff, consider the combination of those
influences as fair warning.

Over the years I have used a variety of traditional
instruments and electronic gear to make, er, um, "music?"
Yes, let's call it "music."

- [Musicography](./musicography.md)

## Studio Setups

- [Yesterday](yesterday.md)
- [Today](today.md)

## Approaches & Techniques

My process for creating music has evolved as new technologies have become
available, but conceptually is not all that different from how I [started
out](./yesterday.md) as a composer. I have loved listening to many types of
music all my life. The first album that I bought with my "own" money was a
recording of Schubert's _Unfinished Symphony_. I had worn the grooves out of it
by the time I turned seven years old. By the time I was in my late teens, I was
writing papers for my degree in Formal Linguistics and, a couple of years later,
teaching myself to program computers while listening to a personal soundtrack
that featured Lou Reed's _Metal Machine Music_ -- yes, the 4-LP sides' worth of
guitar feedback and effects noise that ended his career just as it was starting
to take off in the late 1970's. You will find strong hints of all those
influences - classical and experimental music together with mathematics and a
love of algorithms - throughout my musical _oeuvre_. To me, an analog
synthesizer is a device for translating what are essentially mathematical
formulas into acoustic impulses, which are occasionally and serendipitously
esthetically pleasing (at least to someone with my extremely idiosyncratic
tastes). Computer software makes that even more literally true.

- [Mostly Analog](analog.md)
- [More Than a Little Digital](digital.md)

### Sonic Pi Ruby Code

[Sonic Pi](https://sonic-pi.net/) is programming language, musical instrument
and MIDI sequencer all at the same time.

<template v-for="(file, index) in data">
    <hr v-if="index > 0" />
    <h4>{{ file.file }}</h4>
    <pre>{{ file.contents }}</pre>
</template>
