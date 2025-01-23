# Copyright 2023 Kirk Rader. All rights reserved.

# Running Out the Clock

terminate = false

# track 1
comment do
  
  notes1 = (range 24, 48).reflect
  notes2 = (range 48, 24).reflect
  beats = (spread (notes1.length - 4), notes1.length)
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      2.times do
        4.times do
          notes1.length.times do
            cue :track1
            sleep 0.25
          end
        end
      end
    ensure
      terminate = true
      cue :track1
      sleep 1
      midi_all_notes_off
    end
  end
  
  in_thread do
    with_random_seed 20240210 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 0 do
            loop do
              notes1.length.times do
                sync :track1
                stop if terminate
                if beats.tick
                  midi notes1[0], sustain: 0.125
                  notes1 = notes1.rotate
                end
              end
              notes1 = notes1.shuffle
            end
          end
        end
      end
    end
  end
  
  in_thread do
    with_random_seed 10022024 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 1 do
            loop do
              notes1.length.times do
                sync :track1
                stop if terminate
                if beats.tick
                  midi notes2[0], sustain: 0.125
                  notes2 = notes2.rotate
                end
              end
              notes2 = notes2.shuffle
            end
          end
        end
      end
    end
  end
  
end

# track 2
comment do
  
  notes1 = (range 31, 55).reflect
  notes2 = (range 55, 31).reflect
  beats = (spread (notes1.length - 5), notes1.length)
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      2.times do
        2.times do
          notes1.length.times do
            cue :track2
            sleep 0.5
          end
        end
      end
    ensure
      terminate = true
      cue :track2
      sleep 1
      midi_all_notes_off
    end
  end
  
  in_thread do
    with_random_seed 20240210 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 0 do
            loop do
              notes1.length.times do
                sync :track2
                stop if terminate
                if beats.tick
                  midi notes1[0], sustain: 0.25
                  notes1 = notes1.rotate
                end
              end
              notes1 = notes1.shuffle
            end
          end
        end
      end
    end
  end
  
  in_thread do
    with_random_seed 10022024 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 1 do
            loop do
              notes1.length.times do
                sync :track2
                stop if terminate
                if beats.tick
                  midi notes2[0], sustain: 0.25
                  notes2 = notes2.rotate
                end
              end
              notes2 = notes2.shuffle
            end
          end
        end
      end
    end
  end
  
end

# track 3
uncomment do
  
  notes1 = (range 40, 64).reflect
  notes2 = (range 64, 40).reflect
  beats = (spread (notes1.length - 6), notes1.length)
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      2.times do
        notes1.length.times do
          cue :track3
          sleep 1
        end
      end
    ensure
      terminate = true
      cue :track3
      sleep 1
      midi_all_notes_off
    end
  end
  
  in_thread do
    with_random_seed 20240210 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 0 do
            loop do
              notes1.length.times do
                sync :track3
                stop if terminate
                if beats.tick
                  midi notes1[0], sustain: 0.5
                  notes1 = notes1.rotate
                end
              end
              notes1 = notes1.shuffle
            end
          end
        end
      end
    end
  end
  
  in_thread do
    with_random_seed 10022024 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 1 do
            loop do
              notes1.length.times do
                sync :track3
                stop if terminate
                if beats.tick
                  midi notes2[0], sustain: 0.5
                  notes2 = notes2.rotate
                end
              end
              notes2 = notes2.shuffle
            end
          end
        end
      end
    end
  end
  
end
