# Copyright 2023 Kirk Rader. All rights reserved.

# 12 Lares Compitales

terminate = false

# track 1
uncomment do
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      480.times do
        cue :track1
        sleep 0.25
      end
    ensure
      terminate = true
      cue :track1
      sleep 1
      midi_all_notes_off
    end
  end
  
  in_thread do
    with_random_seed 20240204 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 0 do
            notes = (range 33, 44)
            beats = (spread (notes.length - 3), notes.length)
            loop do
              notes.length.times do
                sync :track1
                stop if terminate
                if beats.tick
                  #play notes[0], amp: 0.5
                  midi notes[0]
                  notes = notes.rotate
                end
              end
              notes = notes.shuffle
            end
          end
        end
      end
    end
  end
  
  in_thread do
    with_random_seed 20240204 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 0 do
            notes = (range 56, 45)
            beats = (spread (notes.length - 3), notes.length)
            loop do
              notes.length.times do
                sync :track1
                stop if terminate
                if beats.tick
                  #play notes[0], amp: 0.5
                  midi notes[0]
                  notes = notes.rotate
                end
              end
              notes = notes.shuffle
            end
          end
        end
      end
    end
  end
  
end

# track 2
comment do
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      240.times do
        cue :track2
        sleep 0.5
      end
    ensure
      terminate = true
      cue :track2
      sleep 1
      midi_all_notes_off
    end
  end
  
  in_thread do
    with_random_seed 20240204 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 1 do
            notes = (range 33, 44).reverse
            beats = (spread (notes.length - 3), notes.length).rotate(2)
            loop do
              notes.length.times do
                sync :track2
                stop if terminate
                if beats.tick
                  #play notes[0], amp: 0.5
                  midi notes[0]
                  notes = notes.rotate
                end
              end
              notes = notes.shuffle
            end
          end
        end
      end
    end
  end
  
  in_thread do
    with_random_seed 20240204 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 1 do
            notes = (range 56, 45).reverse
            beats = (spread (notes.length - 3), notes.length).rotate(2)
            loop do
              notes.length.times do
                sync :track2
                stop if terminate
                if beats.tick
                  #play notes[0], amp: 0.5
                  midi notes[0]
                  notes = notes.rotate
                end
              end
              notes = notes.shuffle
            end
          end
        end
      end
    end
  end
  
end

# track 3
comment do
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      120.times do
        cue :track3
        sleep 1
      end
    ensure
      terminate = true
      cue :track3
      sleep 1
      midi_all_notes_off
    end
  end
  
  in_thread do
    with_random_seed 20240204 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 2 do
            notes = (range 33, 44)
            beats = (spread (notes.length - 3), notes.length).rotate(1)
            loop do
              notes.length.times do
                sync :track3
                stop if terminate
                if beats.tick
                  #play notes[0], amp: 0.5
                  midi notes[0]
                  notes = notes.rotate
                end
              end
              notes = notes.shuffle
            end
          end
        end
      end
    end
  end
  
  in_thread do
    with_random_seed 20240204 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 2 do
            notes = (range 56, 45).reverse
            beats = (spread (notes.length - 3), notes.length).rotate(1)
            loop do
              notes.length.times do
                sync :track3
                stop if terminate
                if beats.tick
                  #play notes[0], amp: 0.5
                  midi notes[0]
                  notes = notes.rotate
                end
              end
              notes = notes.shuffle
            end
          end
        end
      end
    end
  end
  
end
