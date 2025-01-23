# Copyright 2023 Kirk Rader. All rights reserved.

# Consensual Reality

uncomment do
  in_thread do
    with_bpm 200 do
      midi (hz_to_midi 440)
      sleep 10
      notes1 = (range -1.0, 1.0, (2.0 / 16.0)).map {|x| 48 + (24.0 * Math.cos(x)).round}.ring.take(16)
      notes2 = (range 1.0, -1.0, (-2.0 / 16.0)).map {|x| 36 + (12.0 * Math.cos(x)).round}.ring.take(16)
      notes3 = notes1.rotate
      notes4 = notes2.rotate(-1)
      beats1 = (spread (2 * (notes1.length / 3)), notes1.length)
      beats2 = (spread (3 * (notes1.length / 2)), notes1.length)
      beats3 = beats1.reverse
      beats4 = beats2.reverse
      notes1.length.times do
        notes1.length.times do
          tick
          uncomment do
            if beats1.look
              midi notes1[0]
              notes1 = notes1.rotate
            end
          end
          comment do
            if beats2.look
              midi notes2[0]
              notes2 = notes2.rotate
            end
          end
          comment do
            if beats3.look
              midi notes3[0]
              notes3 = notes3.rotate
            end
          end
          comment do
            if beats4.look
              midi notes4[0]
              notes4 = notes4.rotate
            end
          end
          sleep 1
        end
        beats1 = beats1.rotate
        beats2 = beats2.rotate
        beats3 = beats3.rotate
      end
    ensure
      midi_all_notes_off
    end
  end
end