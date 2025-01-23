# Copyright 2023 Kirk Rader. All rights reserved.

# Dorian Light Pink

key = :F2
mode = :dorian
color = :light_pink
track = 4

define :make_beats do |numerator, denomintor, total|
  (spread ((total / denomintor).round * numerator), total)
end

uncomment do
  in_thread do
    with_random_seed 231210 do
      with_random_source color do
        notes = (scale key, mode).reflect.butlast.shuffle
        beats = (make_beats 6, 8, notes.length)
        midi key, sustain: 0.9
        play key, sustain: 0.9
        sleep 10
        with_synth :fm do
          with_bpm 240 do
            20.times do
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
              notes = notes.rotate
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
      with_random_source color do
        notes = (scale key, mode).reflect.butlast.shuffle
        beats = (make_beats 4, 5, notes.length)
        midi key, sustain: 0.9
        play key, sustain: 0.9
        sleep 10
        with_synth :fm do
          with_bpm 240 do
            20.times do
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
              notes = notes.rotate(-1)
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
      with_random_source color do
        notes = (scale key, mode).reflect.butlast.shuffle
        beats = (make_beats 5, 7, notes.length)
        midi key, sustain: 0.9
        play key, sustain: 0.9
        sleep 10
        with_synth :fm do
          with_bpm 240 do
            20.times do
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
              notes = notes.rotate(2)
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
    with_random_seed 231212 do
      with_random_source color do
        notes = (scale key, mode).reflect.butlast.shuffle
        beats = (make_beats 4, 8, notes.length)
        midi key, sustain: 0.9
        play key, sustain: 0.9
        sleep 10
        with_synth :fm do
          with_bpm 240 do
            20.times do
              notes.length.times do
                if beats.tick
                  with_octave 0 do
                    if track == 4
                      midi notes[0], sustain: 0.9
                    else
                      play notes[0], sustain: 0.9, amp: 0.5, pan: -0.2
                    end
                    notes = notes.rotate
                  end
                end
                sleep 1
              end
              notes = notes.rotate(2)
            end
          end
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end