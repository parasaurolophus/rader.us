# Copyright 2023 Kirk Rader. All rights reserved.

# 471

comment do
  # tune midi synth
  loop do
    midi :A4, sustain: 0.99
    synth :fm, note: :A4, sustain: 0.99
    sleep 1
  end
ensure
  midi_all_notes_off
end

bpm = 180

uncomment do
  in_thread do
    with_random_seed 471 do
      with_random_source :perlin do
        with_bpm bpm do
          beats1 = (spread 5, 8)
          beats2 = (spread 4, 7)
          notes1 = (note_range :C3, :B4).reflect
          notes2 = (note_range :B5, :C4).reflect
          count = 0
          6.times do
            notes1 = notes1.shuffle
            notes2 = notes2.shuffle
            notes1.length.times do
              tick
              if beats1.look
                midi notes1.look, sustain: 2
              end
              if beats2.look
                midi notes2.look, sustain: 2
              end
              sleep 1
            end
          end
          midi :C4, sustain: 2
          midi :C5, sustain: 2
          sleep 1
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end