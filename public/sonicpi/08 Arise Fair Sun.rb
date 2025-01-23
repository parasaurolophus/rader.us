# Copyright 2023 Kirk Rader. All rights reserved.

# Arise, Fair Sun

terminate = false

define :drone_thread do |name, seed, min, max, pan|
  in_thread do
    with_random_seed seed do
      with_random_source :pink do
        with_sample_bpm name do
          loop do
            rate = rrand(min, max)
            puts rate
            sample name, rate: rate, pan: pan
            sleep rate
            stop if terminate
          end
        end
      end
    end
  end
end

midi :a4, sustain: 1
synth :sine, note: :a4, sustain: 1
sleep 4

uncomment do
  sample :ambi_glass_rub if false
  drone_thread :ambi_glass_hum, 0, 0.25, 1.0, -0.5
  drone_thread :ambi_glass_rub, 1, 0.5, 1.5, 0.5
end

uncomment do
  in_thread do
    with_random_seed 0 do
      with_random_source :white do
        begin
          scales = (ring
                    (scale :a3, :minor),
                    (scale :c3, :major),
                    (scale :e3, :minor))
          scales_beat = (spread 5, 16)
          tempo = 0.15
          sustain = tempo * 0.9
          15.times do
            notes = scales[0].shuffle
            notes_count = 0
            while notes_count < notes.length
              tick
              if scales_beat.look
                midi notes[notes_count], sustain: sustain
                #synth :kalimba, note: notes[notes_count], sustain: sustain
                notes_count = inc notes_count
              end
              sleep tempo
              scales = scales.rotate
            end
          end
        ensure
          terminate = true
          cue :tick
          midi_all_notes_off
        end
      end
    end
  end
end
