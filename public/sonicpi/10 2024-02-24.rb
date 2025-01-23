# Copyright 2024 Kirk Rader. All rights reserved.

# 2024-02-24

terminate = false
tempo = 120
notes1 = (scale :A1, :harmonic_minor, num_octaves: 4)
notes3 = notes1.reverse
n1 = notes1.length
beats1 = (spread (n1 / 3) * 2, n1)
beats2 = (spread (n1 / 4) * 3, n1)
beats3 = (spread (n1 / 6) * 5, n1)
seed1 = 20240224
seed2 = 2242024
seed3 = 24022024

uncomment do
  in_thread do
    with_bpm tempo do
      midi :A4
      sleep 10
      5.times do
        n1.times do
          cue :master
          sleep 1
        end
      end
    ensure
      terminate = true
      cue :master
      sleep 1
      midi_all_notes_off
      sleep 1
    end
  end
end

uncomment do
  in_thread do
    with_bpm tempo do
      with_random_seed seed3 do
        with_random_source :perlin do
          notes = notes3.shuffle
          beats = beats3.shuffle
          loop do
            stop if terminate
            sync :master
            tick
            midi notes.look, sustain: 0.9 if beats.look
          end
        end
      end
    end
  end
end
