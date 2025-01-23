# Copyright 2023 Kirk Rader. All rights reserved.

# Nautilus

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

define :fibonacci do |n|
  if n < 2
    return 1
  else
    return (fibonacci (n - 1)) + (fibonacci (n - 2))
  end
end

with_bpm 120 do
  with_random_seed 0 do
    with_random_source :perlin do
      with_fx :ring_mod, mix: 0.75, freq_slide: 0.0 do |ring_mod|
        with_synth :sine do
          notes1 = ((range 0, 11).map {|x| 23 + (fibonacci x)}).ring
          notes2 = ((range 0, 11).map {|x| 97 - (fibonacci x)}).ring
          beats1 = (spread 4, 6) + (spread 5, 6).rotate
          beats2 = (spread 9, 11)
          control ring_mod, freq: notes2[0]
          play :A4, sustain: 1
          midi :A4, sustain: 1
          sleep 12
          12.times do
            notes1.length.times do
              tick
              control ring_mod, freq: notes2[0]
              if beats1.look
                play notes1[0], sustain: 0.99, pan: -0.75
                midi notes1[0], sustain: 0.99
                notes1 = notes1.rotate
              end
              if beats2.look
                control ring_mod, freq: notes2[0]
                play notes2[0], sustain: 0.99, pan: 0.5
                midi notes2[0], sustain: 0.99
                notes2 = notes2.rotate
              end
              sleep 1
            end
            notes1 = notes1.shuffle
            notes2 = notes2.shuffle
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end