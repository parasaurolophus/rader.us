# Copyright 2023 Kirk Rader. All rights reserved.

# Rotating Shuffled Minor Triads

comment do
  note = (hz_to_midi 440)
  loop do
    synth :fm, note: note
    midi note
    sleep 1
  end
end

uncomment do
  with_random_seed 231118 do
    with_random_source :white do
      notes1 = (scale :A3, :minor).reflect.butlast.shuffle
      notes2 = notes1.rotate(-2)
      notes3 = notes1.rotate(-4)
      beats1 = (spread (notes1.length - 5), notes1.length)
      beats2 = (spread (notes2.length - 3), notes2.length).rotate(1)
      beats3 = (spread (notes3.length - 2), notes3.length)
      synth :fm, note: (hz_to_midi 440)
      midi (hz_to_midi 440)
      sleep 10
      with_bpm 160 do
        (notes1.length * 1.5).round.times do
          notes1.length.times do
            tick
            if beats1.look
              note = notes1[0] - 12
              synth :fm, note: note, pan: 0.25
              #midi note, sustain: 0.9
              notes1 = notes1.rotate
            end
            if beats2.look
              note = notes2[0]
              synth :fm, note: note, pan: 0.0
              #midi note, sustain: 0.9
              notes2 = notes2.rotate
            end
            if beats3.look
              note = notes3[0] + 12
              synth :fm, note: note, pan: -0.25
              #midi note, sustain: 0.9
              notes3 = notes3.rotate
            end
            sleep 1
          end
        end
      end
    ensure
      midi_all_notes_off
    end
  end
end