# Copyright 2023 Kirk Rader. All rights reserved.

# Yuletide

comment do
  note = (hz_to_midi 440)
  loop do
    synth :fm, note: note
    midi note
    sleep 1
  end
end

uncomment do
  in_thread do
    with_random_seed 0 do
      notes = (scale :C3, :major).reflect.butlast
      midi 48, sustain: 0.9
      sleep 10
      with_random_source :perlin do
        with_bpm 100 do
          12.times do
            notes.length.times do
              midi notes[0], sustain: 0.9
              notes = notes.rotate
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
end
