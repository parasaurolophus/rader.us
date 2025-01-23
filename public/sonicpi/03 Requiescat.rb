# Copyright 2024 Kirk Rader. All rights reserved.

# 03

uncomment do
  in_thread do
    with_random_seed 20240331 do
      with_random_source :white do
        with_synth :blade do
          with_bpm 120 do
            p = 0.5
            s = 1
            notes = (scale :C1, :melodic_minor_asc) + (scale :C1, :melodic_minor_desc).reverse.drop(1)
            notes = notes.take(notes.length - 1)
            beats = (spread 5, 6)
            play (hz_to_midi 440)
            sleep 12
            with_octave 1 do
              12.times do
                notes.length.times do
                  tick
                  (play notes[look], sustain: s, pan: (rrand -0.5, 0.5)) if beats.look
                  notes = notes.rotate
                  sleep 1
                end
                beats = beats.rotate
              end
              play notes[0], sustain: s * 2, pan: p
              sleep s * 2
            end
          end
        end
      end
    end
  end
end

uncomment do
  in_thread do
    with_random_seed 31032024 do
      with_random_source :white do
        with_synth :blade do
          with_bpm 160 do
            p = 0.0
            s = 1
            notes = (scale :C1, :melodic_minor_asc) + (scale :C1, :melodic_minor_desc).reverse.drop(1)
            notes = notes.take(notes.length - 1)
            beats = (spread 4, 5)
            play (hz_to_midi 440)
            sleep 16
            with_octave 2 do
              16.times do
                notes.length.times do
                  tick
                  (play notes[look], sustain: s, pan: (rrand -0.5, 0.5)) if beats.look
                  sleep 1
                end
                beats = beats.rotate
              end
              play notes[0], sustain: s * 2, pan: p
              sleep s * 2
            end
          end
        end
      end
    end
  end
end

uncomment do
  in_thread do
    with_random_seed 3312024 do
      with_random_source :white do
        with_synth :blade do
          with_bpm 90 do
            p = -0.5
            s = 1
            notes = (scale :C1, :melodic_minor_asc) + (scale :C1, :melodic_minor_desc).reverse.drop(1)
            notes = notes.take(notes.length - 1)
            beats = (spread 6, 7)
            play (hz_to_midi 440)
            sleep 9
            with_octave 4 do
              9.times do
                notes.length.times do
                  tick
                  (play notes[look], sustain: s, pan: (rrand -0.5, 0.5)) if beats.look
                  sleep 1
                end
                beats = beats.rotate
              end
              play notes[0], sustain: s * 2, pan: p
              sleep s * 2
            end
          end
        end
      end
    end
  end
end
