# Copyright 2024 Kirk Rader. All rights reserved.

# 10

uncomment do
  
  two_pi = 2 * Math::PI
  x = (range -two_pi, two_pi, (2 * two_pi) / 24)
  notes = x.map{|x| 48 + Math.sin(2 * x) + Math.sin(x / 2)}.take(24)
  n = notes.length / 3
  beats = (spread (n * 2), notes.length) + (spread n, notes.length)
  
  with_bpm 90 do
    midi (hz_to_midi 440), sustain: 0.1
    sleep 9
    9.times do
      notes.length.times do
        tick
        if beats.look
          with_octave 1 do
            midi notes[0], sustain: 0.9
          end
          notes = notes.rotate
        end
        sleep 1
      end
      beats = beats.rotate
    end
  ensure
    midi_all_notes_off
  end
end
