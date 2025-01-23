# Copyright 2023 Kirk Rader. All rights reserved.

# Krampuslauf 2

define :play_notes do |seed, source, tempo, pan, octave, loops, enable_midi, rotate|
  with_random_seed seed do
    with_random_source source do
      notes = (scale :A2, :augmented2).reflect.butlast.rotate(rotate)
      beats = (spread notes.length - 3, notes.length)
      with_synth :fm do
        play :A4, sustain: 0.9, amp: 0.5, pan: pan
        midi :A4, sustain: 0.9
        sleep 10
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
            beats = beats.shuffle
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
    play_notes 20231219, :light_pink, 240, 0.5, -1, 25, false, 0
  end
end

uncomment do
  in_thread do
    play_notes 19122023, :light_pink, 240, 0.0, 0, 25, false, 3
  end
end

uncomment do
  in_thread do
    play_notes 12192023, :light_pink, 240, -0.5, 1, 25, true, 6
  end
end
