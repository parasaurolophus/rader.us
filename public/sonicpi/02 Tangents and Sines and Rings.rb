# Copyright 2023 Kirk Rader. All rights reserved.

# Tangents and Sines and Rings

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
      with_random_source :white do
        with_bpm bpm do
          notes1 = nil
          notes2 = nil
          comment do
            notes1 = (range -Math::PI, Math::PI, step: ((2.0 * Math::PI) / 12.0)).map {|x| 42 + (Math.sin(x) * 12.0).round}
            notes2 = (range -1.0, 1.0, step: (2.0 / 12.0)).map {|x| 66 + (Math.tan(x) * 12.0).round}
          end
          comment do
            notes1 = (range -Math::PI, Math::PI, step: ((2.0 * Math::PI) / 12.0)).map {|x| 66 + (Math.cos(x) * 12.0).round}
            notes2 = (range -1.0, 1.0, step: (2.0 / 12.0)).map {|x| 42 + (Math.atan(x) * 12.0).round}
          end
          uncomment do
            notes1 = (range -Math::PI, Math::PI, step: ((2.0 * Math::PI) / 24.0)).map {|x| 48 + (Math.sin(x) * 24.0).round}
            notes2 = (range -1.0, 1.0, step: (2.0 / 24.0)).map {|x| 24 + (Math.tan(x) * 24.0).round}
          end
          notes1 = notes1.take(notes1.length - 1)
          notes2 = notes2.drop(1)
          #beats = (spread 4, 7)
          #beats = (spread 5, 8)
          beats = (spread 5, 8).rotate
          midi :A4, sustain: 1
          sleep 4
          (bpm * 1.5).round.times do
            tick
            if beats.look
              uncomment do
                midi notes1[0], sustain: 2
                midi notes2[0], sustain: 2
                notes1 = notes1.rotate
                notes2 = notes2.rotate
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
