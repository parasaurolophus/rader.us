# Copyright 2023 Kirk Rader. All rights reserved.

# Baraboo to Dubuque

notes1 = (range -Math::PI, Math::PI, ((2 * Math::PI) / 16)).map {|x| (6 * Math.sin(x)).round + 60}.ring.butlast
notes2 = (range -Math::PI, Math::PI, ((2 * Math::PI) / 15)).map {|x| (6 * Math.cos(x)).round + 48}.ring.butlast
beats1 = (spread (notes1.length - 3), notes1.length)
beats2 = (spread (notes2.length - 2), notes2.length)

use_bpm 200

with_fx :reverb do
  with_fx :ring_mod do
    with_random_seed 0 do
      with_random_source :pink do
        notes2.length.times do
          notes1.length.times do
            tick
            if beats1.look
              synth :beep, note: notes1[0], amp: 0.5, pan: -0.5
              notes1 = notes1.rotate
            end
            if beats2.look
              synth :beep, note: notes2[0], amp: 0.5, pan: 0.5
              notes2 = notes2.rotate
            end
            sleep 1
          end
          notes1 = notes1.shuffle
          notes2 = notes2.shuffle
          beats1 = beats1.rotate
          beats2 = beats2.rotate(-1)
        end
      end
    end
  end
end