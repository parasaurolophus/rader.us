# Copyright 2023 Kirk Rader. All rights reserved.

# Niklaus

define :play_bass do |enable_midi|
  with_octave -1 do
    with_synth :fm do
      notes = (scale :A2, :major).reflect.butlast
      play :A4, sustain: 0.9, amp: 0.5, pan: 0.25
      midi :A4, sustain: 0.9
      sleep 4
      with_bpm 180 do
        phrase = (notes.length - 1)
        notes.length.times do
          notes.length.times do
            if enable_midi
              midi notes[0], sustain: 0.9
            else
              play notes[0], sustain: 0.9, amp: 0.5, pan: 0.25
            end
            notes = notes.rotate(phrase)
            sleep 1
          end
          phrase = (phrase - 1)
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end

define :play_baritone do |enable_midi|
  with_octave 0 do
    with_synth :fm do
      notes = (scale :A2, :major).reflect.butlast.rotate(2)
      play :A4, sustain: 0.9, amp: 0.5, pan: 0.0
      midi :A4, sustain: 0.9
      sleep 4
      with_bpm 180 do
        phrase = 0
        notes.length.times do
          notes.length.times do
            if enable_midi
              midi notes[0], sustain: 0.9
            else
              play notes[0], sustain: 0.9, amp: 0.5, pan: 0.0
            end
            notes = notes.rotate(phrase)
            sleep 1
          end
          phrase = (phrase + 1)
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end

define :play_tenor do |enable_midi|
  with_octave 1 do
    with_synth :fm do
      notes = (scale :A2, :major).reflect.butlast.rotate(4)
      play :A4, sustain: 0.9, amp: 0.5, pan: -0.25
      midi :A4, sustain: 0.9
      sleep 4
      with_bpm 180 do
        phrase = (notes.length / 2)
        notes.length.times do
          notes.length.times do
            if enable_midi
              midi notes[0], sustain: 0.9
            else
              play notes[0], sustain: 0.9, amp: 0.5, pan: -0.25
            end
            notes = notes.rotate(phrase)
            sleep 1
          end
          phrase = (phrase + 1) % notes.length
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end

in_thread do
  play_bass false
end

in_thread do
  play_baritone false
end

in_thread do
  play_tenor true
end