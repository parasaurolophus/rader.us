# More Than a Little Digital

[All of that said](analog.md), I have increased my use over the years of tools
and techniques that simply did not exist when I was young. Some of the sounds on
my albums were produced using completely digital synthesis. These are often in
the form of digital audio samples or the output of VST plugins using software
like _Ableton Live_ and _Sonic Pi_. Using _Ableton Live_, I can play a keyboard
(to within the very narrow limis of my skill) to produce more conventionally
tonal music. Some pieces consist only of tracks recorded in this way, while
others combine analog and digital sound sources in various ways.

For example, the two versions of _Farandole Lamentoso_ on my recent album _The
Modern Temple of Amusement_ were created using a single
[_Sonic Pi_ program](#farandole) generating output in two ways: the "8-bit mix"
consists of a digital audio file generated directly by _Sonic Pi_ using some of
its built-in digital synthesizers. The "2600 remix" uses the _Behringer 2600_
as the sound source, driven by the same _Sonic Pi_ program, but by sending a
sequence of MIDI commands rather than generating digital audio directly. Because
the _2600_ is monophonic, the latter version required running the program
multiple times with slight modifications each time to produce the MIDI sequence
for each voice separately.

Another piece, _Ghostly Reminiscences_, on the same album as the _Farandole_,
was produced entirely as the output of another _Sonic Pi_ program. Even when
using digital tools like _Sonic Pi_, I tend to use randomization and sonic
layering to produce unconventional timbres, rhythms and harmonies that will vary
endlessly no matter how long they are looped. These Ruby source code files are
the closest I am likely to come to writing "scores" for any of my music. This is
my "West Coast" approach applied to using "East Coast" inspired gear and software.

- [Au_Quai.rb](sonicpi/Au_Quai.md)
- [Decommisioned.rb](sonicpi/Decommisioned.md)
- <a id="farandole"></a>[Farandole_Lamentoso.rb](sonicpi/Farandole_Lamentoso.md)
- [Ghostly_Reminiscences.rb](sonicpi/Ghostly_Reminiscences.md)
