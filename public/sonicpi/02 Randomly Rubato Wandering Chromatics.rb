# Copyright 2023 Kirk Rader. All rights reserved.

# Randomly Rubato Wandering Chromatics

uncomment do
  notes = (range 42, 54)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 120 do
    with_random_seed 0 do
      with_random_source :dark_pink do
        with_transpose rrand(-6, 6) do
          12.times do
            notes.length.times do
              sustain = (rrand 0.5, 1.5)
              midi notes[0], sustain: (0.9 * sustain)
              notes = notes.rotate
              sleep sustain
            end
            notes = notes.shuffle
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end

comment do
  notes = (range 72, 24)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 120 do
    with_random_seed 4 do
      with_random_source :pink do
        with_transpose rrand(0, 11) do
          3.times do
            notes = notes.shuffle
            notes.length.times do
              sustain = (rrand 0.5, 1.5)
              midi notes[0], sustain: (0.9 * sustain)
              notes = notes.rotate
              sleep sustain
            end
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end
