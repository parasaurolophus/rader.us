# Musicography

> - <https://kirkrader.bandcamp.com/music>
> - <https://music.apple.com/us/artist/kirk-rader/417090159>
> - <https://music.amazon.com/artists/B004L4HW52/kirk-rader>
> - <https://music.youtube.com/channel/UCp__q4DYBXYhq9uiD2Y8vUg>
> - <https://open.spotify.com/artist/06lMz4EjJn3pYej2kGIL5t>
>
> _Or search for "Kirk Rader" on the music service you prefer._

<!-- toc -->

I started creating electronic music on the West Coast, in the "West Coast Style," long before I knew that there was such a thing. My musical tastes have always been outside the mainstream. The first album I purchased with my "own" money was a vinyl recording (there were no other commercially significant recording media for sale in the mid 1960's) of Schubert's _Unfinished Symphony_ which I had worn the grooves out of by the time I turned 7 years old. When I was studying formal linguistics and teaching myself to program computers in my late teens and early twenties, I did so to a personal soundtrack that featured Lou Reed's _Metal Machine Music_ (yes, the album consisting of 4 LP sides-worth of electric guitar feedback and effects pedal noise that literally ended his career just as it was beginning to take off in the mid 70's and then went on to become the stuff of legend decades later).

My favorite composers include Tallis, Monteverdi, Vivaldi, Bach, Handel, Beethoven, Glass and Byrne as well as hard-core mid-century experimentalists like Stockhausen and Subotnick. If you have never heard any of my own stuff, consider the combination of those influences as fair warning.

| Album                | Title                                                 | Released                                     |
|----------------------|-------------------------------------------------------|----------------------------------------------|
| ![](Undecidable.png) | _Undecidable_ (EP)                                    | 2011<br>(composed & recorded<br>1978 - 1985) |
| ![](Vivimus.png)     | _Dum Vivimus_                                         | 2023                                         |
| ![](Untitled.png)    | _Untitled_ (EP)                                       | 2023                                         |
| ![](Songs.png)       | _Songs Not Yet and No Longer Sung_                    | 2023                                         |
| ![](March.png)       | _March_ (EP)                                          | 2023                                         |
| ![](April.png)       | _April_                                               | 2023                                         |
| ![](Chatbots.png)    | _No Chatbots Were Harmed in the Making of This Album_ | 2023                                         |
| ![](Ambivalence.png) | _Ambivalence_                                         | 2023                                         |
| ![](Modern.png)      | _The Modern Temple of Amusement_                      | 2023                                         |

---

Over the years I have used a variety of traditional
instruments and electronic gear to make, er, um, "music?"
Yes, let's call it "music."

## Yesterday

Here is the setup I used when creating tracks in the 70's and 80's, a few of which eventually were published as the album _Undecidable_ in 2011, once self-publishing music started to become a thing:

```mermaid
graph LR

    subgraph "&nbsp;"

        subgraph early["Early Days"]
            piano["Piano"]
            arp2600["ARP 2600"]
            mic["Microphone"]
            portastudio["TASCAM PortaStudio 244"]
        end

        subgraph later["Added Over Time"]
            ob8["Oberheim OB-8"]
            dx7["Yamaha DX7"]
        end

        cassette(["Cassette Tape"])

        piano --> mic -- "analog\naudio" ---> portastudio
        arp2600 -- "analog\naudio" --> portastudio
        ob8 -- "analog\naudio" -----> portastudio
        dx7 -- "analog\naudio" -----> portastudio
        portastudio -- "analog\naudio" --> cassette

        classDef group stroke-dasharray: 5 5
        class early,later group

    end
```

- [Wm Knabe & Co Piano](https://www.knabepianos.com)
- [ARP 2600](https://en.wikipedia.org/wiki/ARP_2600)
- [Oberheim OB-8](https://en.wikipedia.org/wiki/Oberheim_OB-8)
- [Yamaha DX7](https://en.wikipedia.org/wiki/Yamaha_DX7)
- [TASCAM PortaStudio 244](https://en.wikipedia.org/wiki/Portastudio)

The _Knabe_ piano I had as a kid was built in the 1930's and sounded far better in real life than on the surviving cassette recordings. Most of these recordings were made in my childhood bedroom or various apartments I had as a college student and young adult using decidedly consumer-level microphones and recording technologies of the era. The jankiness of the audio quality appeals to me as an artifact of the period of my life and the circumstances in which they were created, but I cannot argue with anyone who finds it less charming than I.

---

## Today

The albums I have been publishing more recently have been created using variations of this setup:

```mermaid
graph LR

    subgraph "&nbsp;"

        keyboard[\"Akai Pro MPK261"/]
        b2600["Behringer 2600"]

        eurorack["Various\nEurorack\nModules"]

        adc["PreSonus AudioBox iTwo"]

        subgraph computer["Computer"]
            audacity["Audacity"]
            ableton["Ableton Live"]
            sonicpi["Sonic Pi"]
            plugins["VST"]
            flac[("FLAC,\nWAV,\nMP3")]
            plugins <--> audacity --> flac
            plugins <--> ableton --> flac
            sonicpi --> flac
        end

        monitor[/"PA / Headphones"\]

        subgraph cloud["Online Publishers"]
            cdbaby[/"cdbaby.com"\]
            bandcamp[/"bandcamp.com"\]
        end

        keyboard -- "MIDI\n(DIN)" --> b2600
        keyboard -- "MIDI\n(USB)" --> computer
        b2600 <-- "trigger,\ngate,\ncontrol voltage,\nanalog audio" --> eurorack
        b2600 -- "analog\naudio" --> adc
        adc <-- "digital audio\n(USB)" --> computer
        adc -- "monitor" --> monitor
        computer -- "publish" ----> cdbaby
        computer -- "publish" ----> bandcamp
    end
```

### Hardware

- [Akai Pro MPK261](https://www.akaipro.com/mpk261) keyboard
- [Behringer 2600](https://www.behringer.com/product.html?modelCode=P0DNJ) clone of the _ARP 2600_ I used as a kid
- [PreSonus AudioBox iTwo](https://www.presonus.com/en-US/interfaces/usb-audio-interfaces/audiobox-series/2777700108.html) ADC

...plus various Eurorack modules that change embarassingly frequently

### Software

- [Audacity](https://www.audacityteam.org/) DAW
- [Ableton Live](https://www.ableton.com/) real-time digital synthesis and effects
- [Sonic Pi](https://sonic-pi.net/) Ruby dialect and runtime platform for creating music

...running on a creaky old MacBook Pro laptop

---

## Mostly Analog

The majority of my pieces are created using a process not very different from how I worked in the 70's. I usually begin with a concept for a modular synth patch. These are usually pretty basic notions of ways to connect (and sometimes cross-connect) modules in the signal and control paths, such as "use the interference pattern of two low-frequency oscillators to create a rhythm," "use pitch sweeps driven by an envelope generator on the inputs to a ring modulator to create dynamic timbre" and the like. I then simply play around with such patches -- changing the underlying wave forms, inverting the phase of some of the low-frequency oscillator signals, changing the base frequency relationships when using FM, and so on -- until I find something that appeals to my very idiosyncratic tastes sufficiently to be worth recording. I then repeat the same process to create multiple layers of sound for a given composition. Where I used a multitrack analog recorder back in the day, I now use _Audacity_ to record each sonic layer. Finally, I align and trim the tracks, add fades and similar basic editing in the DAW to produce a final mix as a FLAC file. That is where the greater flexibility of a DAW shines, but I do miss some of the tricks you could play with tape. One of these days I'll have to find or write a time-reversal plugin for _Audacity_ that would work like turning the cassette over between recording two tracks on my old _PortaStudio_.

Most of the time, when I "play" a piece during the recording process it is by adjusting potentiometers to alter the patch in real time. Often I don't do even that during the recording but, instead, allow the "logic" of a given patch to play itself out over time. When I use a keyboard at all when working this way, it is usually to provide trigger and gate voltages rather than to play "notes" in the traditional sense. One characteristic of modular analog synthesis is that small changes to a given patch can produce dramatic differences in the resulting sound. Multiple tracks on a given album may be the result of the process I just described, using relatively minor variations of a single patch.

An aspect of this approach is that there usually is neither a musical score in the conventional sense nor any way to recover the exact combinations of settings and initial phase relationships of the various modules contributing to a patch. Once a track is recorded, that recording is the only representation of that exact "composition" and there would be no way for me or anyone else to perform it again and get the same result. This is one of the features of "West Coast" analog electronic music that I find conceptually attractive. There is a complete break not only from traditional tunings, structures and forms but the totality of a musical composition created in this way is represented entirely by its acoustic signature and nothing else.

---

# More Than a Little Digital

All of that said, I have increased my use over the years of tools and techniques that simply did not exist when I was young. Some of the sounds on my albums were produced using completely digital synthesis. These are often in the form of digital audio samples or the output of VST plugins using software like _Ableton Live_ and _Sonic Pi_. Using _Ableton Live_, I can play a keyboard (to within the very narrow limis of my skill) to produce more conventionally tonal music. Some pieces consist only of tracks recorded in this way, while others combine analog and digital sound sources in various ways.

For example, the two versions of _Farandole Lamentoso_ on my recent album _The Modern Temple of Amusement_ were created using a single [_Sonic Pi_ program](#farandole) generating output in two ways: the "8-bit mix" consists of a digital audio file generated directly by _Sonic Pi_ using some of its built-in digital synthesizers. The "2600 remix" uses the _Behringer 2600_ as the sound source, driven by the same _Sonic Pi_ program, but by sending a sequence of MIDI commands rather than generating digital audio directly. Because the _2600_ is monophonic, the latter version required running the program multiple times with slight modifications each time to produce the MIDI sequence for each voice separately:

<a id="farandole"></a>

```ruby
# Copyright 2023 Kirk Rader. All rights reserved.

# Farandole Lamentoso

midi_all_notes_off
sleep 1

arp_synth = :dpulse
bass_synth = :bass_foundation
seed = 0
next_seed = 7
arps = 2
arp_tempo = 0.33
send_midi_arp = false
send_midi_bass = false
play_notes = true

define :play_arp do |r, c|
  
  use_synth arp_synth
  
  c.times do
    
    use_random_seed seed
    seed = seed + next_seed
    
    r.shuffle.length.times do
      
      n = r.tick
      
      if send_midi_arp
        midi n
      end
      
      if play_notes
        play n
      end
      
      sleep arp_tempo
      
    end
  end
end

define :play_bass do |n, s|
  
  use_synth bass_synth
  
  if send_midi_bass
    midi n, sustain: s
  end
  
  if play_notes
    play n, sustain: s
  end
  
end

define :play_bass_melody do |n, s|
  
  in_thread do
    
    n.length.times do
      
      play_bass n.tick, s
      sleep s
      
    end
  end
end

a_min = (ring :A3, :C4, :E4)
d_min = (ring :A3, :D4, :F4)
g_maj = (ring :G3, :B3, :D4)
c_maj = (ring :G3, :C4, :E4)
f_maj = (ring :F3, :A3, :C4)
b_min = (ring :F3, :B3, :D4)
a_min_inv = (ring :E3, :A3, :C4)
e_maj_sus = (ring :E3, :A3, :B3)
e_maj = (ring :E3, :Gs3, :B3)

bass_melody = (ring :C3, :A2, :B2, :Gs2)

2.times do
  
  play_bass :A3, (arps * a_min.length * arp_tempo)
  play_arp a_min, arps
  
  play_bass :D3, (arps * d_min.length * arp_tempo)
  play_arp d_min, arps
  
  play_bass :G3, (arps * g_maj.length * arp_tempo)
  play_arp g_maj, arps
  
  play_bass :C3, (arps * c_maj.length * arp_tempo)
  play_arp c_maj, arps
  
  play_bass :F3, (arps * f_maj.length * arp_tempo)
  play_arp f_maj, arps
  
  play_bass :B3, (arps * b_min.length * arp_tempo)
  play_arp b_min, arps
  
  play_bass_melody bass_melody, (a_min_inv.length * arp_tempo)
  play_arp a_min_inv, arps
  
  play_arp e_maj_sus, 1
  play_arp e_maj, 1
  
end

play_bass :A2, (arps * a_min_inv.length * arp_tempo)
play_arp a_min_inv, arps

if send_midi_arp
  midi :A4
end

if play_notes
  play :A4
end

play_bass :A2, (arps * a_min_inv.length * arp_tempo)
```

---

Another piece, _Ghostly Reminiscences_, on the same album as the _Farandole_, was produced entirely as the output of another _Sonic Pi_ program:

```ruby
# Copyright 2023 Kirk Rader. All rights reserved.

# Ghostly Reminiscences

live_loop :plingk do
    sample :perc_bell, rate: rrand(0.2, 2)
    sleep rrand(1, 5)
  end
  
  live_loop :clangk do
    sample :perc_bell2, rate: rrand(0.2, 2)
    sleep rrand(1, 5)
  end
  
  live_loop :eery do
    sample :ambi_haunted_hum, rate: rrand(0.2, 2)
    sleep rrand(0.75, 3)
  end
  
  live_loop :spooky do
    sample :ambi_glass_hum, rate: rrand(0.2, 2)
    sleep rrand(0.75, 3)
  end
  
  live_loop :chugga do
    sample :loop_industrial, rate: rrand(0.2, 2)
    sleep 5
  end
  
  live_loop :bzzz do
    sleep 2.5
    sample :loop_drone_g_97, rate: rrand(0.2, 2)
    sleep 7.5
  end
```

You can see that even when using digital tools like _Sonic Pi_, I tend to use randomization and sonic layering to produce unconvential timbres, rhythms and harmonies that will vary endlessly no matter how long they are looped. These Ruby source code files are the closest I am likely to come to writing "scores" for any of my music. This is my "West Coast" approach applied to using "East Coast" inspired gear and software.