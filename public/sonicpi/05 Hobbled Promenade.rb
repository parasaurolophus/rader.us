# Copyright 2024 Kirk Rader. All rights reserved.

# 05 Hobbled Promenade

define :sum_of_sines do |seed, b|
  with_random_seed seed do
    with_random_source :white do
      with_synth :blade do
        with_bpm 120 do
          twopi = (2 * Math::PI)
          midi (hz_to_midi 440)
          sleep 12
          12.times do
            k = (rrand 0.5, 1.5)
            a1 = (rrand -6.0, 6.0)
            b1 = (rrand -6.0, 6.0)
            a2 = (rrand -6.0, 6.0)
            b2 = (rrand -6.0, 6.0)
            c = (rrand_i 36, 60)
            notes = (range 0, twopi, (twopi / 12))
            beats = (spread (notes.length / b) * (b - 1), notes.length).shuffle
            notes.length.times do
              tick
              if beats.look
                x = notes[look]
                y = k * (((a1 * Math.sin(b1 * x)) + (a2 * Math.sin(b2 * x)) + c))
                midi y.round
                notes = notes.rotate
              end
              sleep 1
            end
          end
        end
      end
    ensure
      midi_all_notes_off
    end
  end
end

comment do
  in_thread do
    sum_of_sines 20240505, 4
  end
end

uncomment do
  in_thread do
    sum_of_sines 5052024, 3
  end
end

