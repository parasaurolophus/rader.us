# Copyright 2023 Kirk Rader. All rights reserved.

# Mixed Hesitant Tempos

define :play_notes do |seed, source, tempo, pan, octave, loops, enable_midi|
  with_random_seed seed do
    with_random_source source do
      notes = (scale :A2, :minor).reflect.butlast
      beats = (spread notes.length - 1, notes.length)
      with_synth :fm do
        with_bpm tempo do
          loops.times do
            notes.length.times do
              with_octave octave do
                if beats.tick
                  if enable_midi
                    midi notes[0], sustain: 0.9
                  else
                    play notes[0], sustain: 0.9, amp: 0.5, pan: pan
                  end
                end
                notes = notes.rotate
              end
              sleep 1
            end
            notes = notes.shuffle
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end

uncomment do
  in_thread do
    play_notes 20231213, :perlin, 90, 0.5, -1, 10, false
  end
end

uncomment do
  in_thread do
    play_notes 13122023, :perlin, 135, 0.0, 0, 15, false
  end
end

uncomment do
  in_thread do
    play_notes 12132023, :perlin, 180, -0.5, 1, 20, false
  end
end
