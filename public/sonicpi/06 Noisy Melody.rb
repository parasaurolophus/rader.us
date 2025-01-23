# Copyright 2023 Kirk Rader. All rights reserved.

# Noisy Melody

comment do
  # tune midi synth
  loop do
    midi :A4, sustain: 3
    synth :fm, note: :A4, sustain: 3
    sleep 3
  end
ensure
  midi_all_notes_off
end

uncomment do
  with_bpm 200 do
    with_random_seed 0 do
      #with_random_source :perlin do
      #with_random_source :pink do
      with_random_source :white do
        beats1 = (spread 9, 16)
        beats2 = (spread 4, 7)
        midi :A4, sustain: 1
        sleep 16
        300.times do
          tick
          duration = (rrand 0.5, 1.5)
          sustain = (duration * 0.99)
          note1 = (rrand_i 24, 47)
          note2 = (rrand_i 48, 84)
          (midi note1, sustain: sustain) if beats1.look
          (midi note2, sustain: sustain) if beats2.look
          sleep duration
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end
