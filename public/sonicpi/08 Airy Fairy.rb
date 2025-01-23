# Copyright 2023 Kirk Rader. All rights reserved.

# Airy Fairy

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

uncomment do
  with_bpm 200 do
    with_random_seed 0 do
      #with_random_source :perlin do
      #with_random_source :pink do
      with_random_source :white do
        beats1 = (spread 3, 8) + (ring false).repeat(8)
        beats2 = (spread 4, 7).rotate + (ring false).repeat(7)
        beats3 = (ring false).repeat(8) + (spread 5, 8)
        midi :A4, sustain: 1
        sleep 16
        600.times do
          tick
          #(midi rrand_i(24, 84), sustain: 0.99) if beats1.look
          #(midi rrand_i(36, 72), sustain: 0.99) if beats2.look
          (midi rrand_i(48, 60), sustain: 0.99) if beats3.look
          sleep 1
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end
