# Copyright 2023 Kirk Rader. All rights reserved.

# l'Enfant Terrible

terminate = false

notes1 = (scale :A2, :harmonic_minor, 2)
beats1 = (spread (notes1.length - 5), notes1.length)

notes2 = (scale :A2, :harmonic_minor, 1).mirror
beats2 = (spread (notes2.length - 4), notes2.length)

notes3 = (scale :A2, :harmonic_minor, 2).reverse
beats3 = (spread (notes3.length - 3), notes3.length)

# track 1
uncomment do
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      10.times do
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
          with_octave 2 do
            loop do
              notes1.length.times do
                sync :track1
                stop if terminate
                if beats1.tick
                  #play notes1[0], amp: 0.5
                  midi notes1[0]
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
  
end

# track 2
comment do
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      10.times do
        2.times do
          notes3.length.times do
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
    with_random_seed 10042024 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 1 do
            loop do
              notes2.length.times do
                sync :track2
                stop if terminate
                if beats2.tick
                  #play notes2[0], amp: 0.5
                  midi notes2[0]
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
comment do
  
  in_thread do
    with_bpm 60 do
      synth :fm, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 10
      10.times do
        1.times do
          notes3.length.times do
            cue :track3
            sleep 1
          end
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
    with_random_seed 2102024 do
      with_random_source :pink do
        with_synth :fm do
          with_octave 0 do
            loop do
              notes3.length.times do
                sync :track3
                stop if terminate
                if beats3.tick
                  #play notes3[0], amp: 0.5
                  midi notes3[0]
                  notes3 = notes3.rotate
                end
              end
              notes3 = notes3.shuffle
            end
          end
        end
      end
    end
  end
  
end
