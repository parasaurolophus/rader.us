# Copyright 2023 Kirk Rader. All rights reserved.

# Black and White and Pink All Over

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
    with_random_seed 0 do
      #with_random_source :white do
      with_random_source :pink do
        with_bpm bpm do
          #beats = (spread 5, 8)
          beats = (spread 5, 8).rotate
          midi :A4, sustain: 1
          sleep 4
          (bpm * 2).times do
            tick
            if beats.look
              comment do
                midi (rrand_i 60, 84), sustain: 2
                midi (rrand_i 24, 48), sustain: 2
              end
              uncomment do
                midi (rrand_i 60, 72), sustain: 2
                midi (rrand_i 36, 48), sustain: 2
              end
            end
            sleep 1
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end
