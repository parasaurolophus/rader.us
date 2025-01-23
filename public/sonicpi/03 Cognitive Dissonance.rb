# Copyright 2023 Kirk Rader. All rights reserved.

# Cognitive Dissonance

uncomment do
  #notes = (range 42, 54)
  #notes = (range 54, 42)
  #notes = (range 45, 52).reflect.butlast
  notes = (range 41, 55, 2).reflect.butlast
  #beats = (spread ((notes.length / 4).round * 3), notes.length)
  #beats = (spread ((notes.length / 6).round * 5), notes.length)
  #beats = (spread (notes.length / 4).round, notes.length)
  #beats = (spread (notes.length / 6).round, notes.length)
  #beats = (spread ((notes.length / 3).round * 2), notes.length)
  #beats = beats.rotate
  beats = (spread (notes.length - 1), notes.length)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 120 do
    with_random_seed 0 do
      with_random_source :pink do
        notes.length.times do
          notes.length.times do
            tick
            if beats.look
              with_transpose 0 do
                midi notes[0], sustain: 0.9
              end
              notes = notes.rotate
            end
            sleep 1
          end
          notes = notes.shuffle
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end
