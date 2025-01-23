# Copyright 2023 Kirk Rader. All rights reserved.

# Aeolian Counterpoint

key = :F2
mode = :aeolian
track = 0

define :make_beats do |numerator, denomintor, total|
  (spread ((total / denomintor).round * numerator), total)
end

uncomment do
  in_thread do
    with_random_seed 231210 do
      with_random_source :pink do
        notes = (scale key, mode).reflect.butlast.shuffle
        beats = (make_beats 3, 4, notes.length)
        midi key, sustain: 0.9
        play key, sustain: 0.9
        sleep 10
        with_synth :fm do
          with_bpm 180 do
            15.times do
              notes.length.times do
                if beats.tick
                  with_octave -1 do
                    if track == 1
                      midi notes[0], sustain: 0.9
                    else
                      play notes[0], sustain: 0.9, amp: 0.5, pan: 0.2
                    end
                    notes = notes.rotate
                  end
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
  end
end

uncomment do
  in_thread do
    with_random_seed 121023 do
      with_random_source :pink do
        notes = (scale key, mode).reflect.butlast.shuffle
        beats = (make_beats 3, 5, notes.length).rotate(2)
        midi key, sustain: 0.9
        play key, sustain: 0.9
        sleep 10
        with_synth :fm do
          with_bpm 180 do
            15.times do
              notes.length.times do
                if beats.tick
                  with_octave 0 do
                    if track == 2
                      midi notes[0], sustain: 0.9
                    else
                      play notes[0], sustain: 0.9, amp: 0.5, pan: 0.0
                    end
                    notes = notes.rotate
                  end
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
  end
end

uncomment do
  in_thread do
    with_random_seed 101223 do
      with_random_source :pink do
        notes = (scale key, mode).reflect.butlast.shuffle
        beats = (make_beats 2, 3, notes.length).rotate(1)
        midi key, sustain: 0.9
        play key, sustain: 0.9
        sleep 10
        with_synth :fm do
          with_bpm 180 do
            15.times do
              notes.length.times do
                if beats.tick
                  with_octave 1 do
                    if track == 3
                      midi notes[0], sustain: 0.9
                    else
                      play notes[0], sustain: 0.9, amp: 0.5, pan: -0.2
                    end
                    notes = notes.rotate
                  end
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
  end
end