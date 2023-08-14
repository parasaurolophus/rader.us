# Today

<!-- toc -->

The albums I have been publishing more recently have been created using
variations of this setup:

```mermaid
graph TB

        subgraph "My Basement"

            keyboard["Akai Pro MPK261"]
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

            monitor["PA / Headphones"]

        end

        subgraph cloud["Online Publishers"]
            cdbaby["cdbaby.com"]
            bandcamp["bandcamp.com"]
        end

        subgraph "Streaming Services (plus many more not shown)"
            itunes["iTunes"]
            youtube["YouTube Music"]
            spotify["Spotify"]
            amazon["Amazon Music"]
        end

        subgraph "Licensing & Royalty Collection"
            soundexchange["SoundExchange"]
        end

        keyboard -- "MIDI\n(DIN)" --> b2600
        b2600 <-- "trigger,\ngate,\ncontrol voltage,\nanalog audio" --> eurorack
        b2600 -- "analog\naudio" --> adc
        adc <-- "digital audio,\nMIDI\n(USB)" --> computer
        adc -- "monitor" --> monitor
        computer <-- "MIDI\n(USB)" --> keyboard
        computer -- "publish" --> cdbaby
        computer -- "publish" --> bandcamp
        cdbaby -- "distribute" --> itunes
        cdbaby -- "distribute" --> youtube
        cdbaby -- "distribute" --> spotify
        cdbaby -- "distribute" --> amazon
        cdbaby -- "register" --> soundexchange
```

## Hardware

- [Akai Pro MPK261](https://www.akaipro.com/mpk261) keyboard
- [Behringer 2600](https://www.behringer.com/product.html?modelCode=P0DNJ)
  modern clone of the _ARP 2600_ I used as a kid
- [PreSonus AudioBox iTwo](https://www.presonus.com/en-US/interfaces/usb-audio-interfaces/audiobox-series/2777700108.html)
  ADC

...plus various Eurorack modules that change embarassingly frequently

## Software

- [Audacity](https://www.audacityteam.org/) Digital Audio Workstation (DAW)
- [Ableton Live](https://www.ableton.com/) real-time digital synthesis and effects
- [Sonic Pi](https://sonic-pi.net/) Ruby dialect and runtime platform for
  creating digital music and controlling MIDI instruments

...running on a creaky old MacBook Pro laptop
