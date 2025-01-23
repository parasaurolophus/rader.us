# Copyright Kirk Rader 2023. All rights reserved.

# House On the Rock

pi10 = (10.0 * Math::PI)
xstep = (pi10 / 36.0)

notes1 = (range 0.0, pi10, xstep).map {|x| 48 + x.round.gcd(48)}.ring.butlast
notes2 = (range 0.0, pi10, xstep).map {|x| 48 - (12.0 * Math.sin(x / Math::PI)).round}.ring

beats1 = (spread (2 * (notes1.length / 3)), notes1.length)
beats2 = (spread (3 * (notes2.length / 4)), notes2.length)
beats3 = (spread (notes1.length / 3) - 1, notes1.length)
beats4 = (spread (notes2.length / 4) + 1, notes2.length)

use_bpm 200

begin
  with_random_seed 0 do
    with_random_source :pink do
      10.times do
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
            midi (range :C1, :C3, pitches: (scale :C, :major)).choose
          end
          if beats4.look
            midi (range :A2, :A4, pitches: (scale :A, :minor)).choose
          end
          sleep 1
        end
        notes1 = notes1.shuffle
        notes2 = notes2.shuffle
      end
    end
  end
ensure
  midi_all_notes_off
end
