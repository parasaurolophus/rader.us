# Copyright 2023 Kirk Rader. All rights reserved.

# Analogy in C Major Ring Tone

chords = (ring
          (ring :C3) + (chord :C5, :maj),
          (ring :F3) + (chord :F4, :maj, invert: 2),
          (ring :G3) + (chord :G4, :maj, invert: 1),
          (ring :B3) + (chord :B4, :dim),
          (ring :C4) + (chord :C4, :sus4, invert: 1),
          (ring :C4) + (chord :C4, :maj, invert: 1)
          )

rates = (ring
         2,
         2,
         1,
         1,
         1,
         1
         )

enable_midi = true
enable_arpeggio = false
enable_bass = true

define :play_arpeggio do |c, r, l|
  in_thread do
    with_random_seed 0 do
      with_random_source :white do
        l.times do
          count = 0
          c = c.shuffle
          while count < c.length
            if enable_midi
              midi c[count], sustain: r * 0.9  if enable_arpeggio
            else
              synth :chiplead, note: c[count], sustain: r * 0.9 if enable_arpeggio
            end
            count = inc count
            sleep r
          end
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end

uncomment do
  in_thread do
    with_random_seed 0 do
      with_random_source :white do
        midi :A4
        synth :pretty_bell, note: :A4
        sleep 4
        count = 0
        3.times do
          chords.length.times do
            tick
            repeats = (rates.look * 2)
            duration = (chords.length * 0.15 * rates.look)
            play_arpeggio chords.look, 0.15, repeats
            with_octave -1 do
              if enable_bass
                (midi (chords.look)[0], sustain: (duration * 0.9)) if ((count > 0) && enable_bass)
              else
                (synth :chipbass, note: (chords.look)[0], sustain: (duration * 0.9)) if ((count > 0) && enable_bass)
              end
              sleep duration
            end
          end
          count = inc count
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end