# Copyright 2023 Kirk Rader. All rights reserved.

# Driftless

notes1 = (range -Math::PI, Math::PI, ((2 * Math::PI) / 16)).map {|x| (8 * Math.cos(x / 2)).round + 64}.ring.butlast
notes2 = (range -Math::PI, Math::PI, ((2 * Math::PI) / 15)).map {|x| (6 * Math.cos(x)).round + 44}.ring.butlast
notes3 = (range -Math::PI, Math::PI, ((2 * Math::PI) / 16)).map {|x| (12 * Math.cos(x / 4)).round + 64}.ring.butlast
beats1 = (spread (notes1.length - 3), notes1.length)
beats2 = (spread (notes2.length - 2), notes2.length)
beats3 = notes1.reverse

use_bpm 200

with_random_seed 0 do
  with_random_source :pink do
    begin
      count = 0
      16.times do
        notes1.length.times do
          tick
          if beats1.look
            midi notes1[0]
            notes1 = notes1.rotate
          end
          if beats2.look
            midi notes2[0]
            notes2 = notes2.rotate
          end
          if beats3.look
            midi notes3[0]
            notes3 = notes3.rotate
          end
          sleep 1
        end
        notes1 = notes1.shuffle
        notes2 = notes2.shuffle
        notes3 = notes3.shuffle
        beats1 = beats1.rotate
        beats2 = beats2.rotate(-1)
        beats3 = beats3.rotate(2)
      end
    ensure
      sleep notes1.length
      midi_all_notes_off
    end
  end
end
