# Copyright 2023 Kirk Rader. All rights reserved.

# Mixolydian Harmonies

key = :F2
mode = :mixolydian

uncomment do
  in_thread do
    with_random_seed 0 do
      notes = (scale key, mode).reflect.butlast #.shuffle
      beats = (spread (notes.length - 3), notes.length)
      midi key, sustain: 0.9
      play key, sustain: 0.9
      sleep 10
      with_random_source :pink do
        with_synth :fm do
          with_bpm 60 do
            5.times do
              notes.length.times do
                if beats.tick
                  with_octave -1 do
                    #midi notes[0], sustain: 0.9
                    play notes[0], sustain: 0.9, amp: 0.5, pan: 0.2
                    notes = notes.rotate
                  end
                end
                sleep 1
              end
              notes = notes.rotate(1)
              beats = beats.rotate(-1)
            end
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
      notes = (scale key, mode).reflect.butlast #.shuffle
      beats = (spread (notes.length - 2), notes.length)
      midi key, sustain: 0.9
      play key, sustain: 0.9
      sleep 10
      with_random_source :pink do
        with_synth :fm do
          with_bpm 120 do
            10.times do
              notes.length.times do
                if beats.tick
                  with_octave 0 do
                    #midi notes[0], sustain: 0.9
                    play notes[0], sustain: 0.9, amp: 0.5, pan: 0.0
                    notes = notes.rotate
                  end
                end
                sleep 1
              end
              #notes = notes.shuffle
            end
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
      notes = (scale key, mode).reflect.butlast #.shuffle
      beats = (spread (notes.length - 1), notes.length)
      midi key, sustain: 0.9
      play key, sustain: 0.9
      sleep 10
      with_random_source :pink do
        with_synth :fm do
          with_bpm 180 do
            15.times do
              notes.length.times do
                if beats.tick
                  with_octave 1 do
                    #midi notes[0], sustain: 0.9
                    play notes[0], sustain: 0.9, amp: 0.5, pan: -0.2
                    notes = notes.rotate
                  end
                end
                sleep 1
              end
              notes = notes.rotate(-1)
              beats = beats.rotate(1)
            end
          end
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end