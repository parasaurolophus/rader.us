# Copyright 2023 Kirk Rader. All rights reserved.

# Intended Consequences

comment do
  begin
    loop do
      synth :sine, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 1
    end
  ensure
    midi_all_notes_off
  end
end

uncomment do
  in_thread do
    with_bpm 200 do
      notes1 = (range -1.0, 1.0, (2.0 / 16.0)).map {|x| 48 + (24.0 * Math.tan(x)).round}.ring.take(16)
      notes2 = (range 1.0, -1.0, (-2.0 / 16.0)).map {|x| 36 + (12.0 * Math.tan(x)).round}.ring.take(16)
      beats1 = (spread (notes1.length - 3), notes1.length)
      beats2 = (spread (notes1.length - 2), (notes1.length - 1))
      beats3 = beats1.reverse
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
              midi notes1[0]
              notes1 = notes1.rotate
            end
          end
          comment do
            if beats3.look
              midi notes2[0]
              notes2 = notes2.rotate
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