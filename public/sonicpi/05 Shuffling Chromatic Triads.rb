# Copyright 2023 Kirk Rader. All rights reserved.

# Shuffling Chromatic Triads

comment do
  note = (hz_to_midi 440)
  loop do
    synth :fm, note: note
    midi note
    sleep 1
  end
end

uncomment do
  in_thread do
    with_random_seed 231122 do
      with_random_source :perlin do
        with_synth :fm do
          notes1 = (range 36, 59) + (range 58, 37)
          notes2 = notes1.rotate(-2)
          notes3 = notes1.rotate(-4)
          beats1 = (spread ((notes1.length / 3).round * 2), notes1.length)
          beats2 = (spread ((notes2.length / 4).round * 3), notes2.length)
          beats3 = (spread ((notes3.length / 5).round * 4), notes3.length).rotate(-1)
          play (hz_to_midi 440), amp: 0.5
          midi (hz_to_midi 440)
          sleep 10
          with_bpm 200 do
            count1 = count2 = count3 = 0
            (notes1.length / 3).round.times do
              notes1.length.times do
                tick
                if beats1.look
                  note = notes1[0] - 12
                  play note, pan: -0.1, sustain: 0.9, amp: 0.5
                  #midi note, sustain: 0.9
                  notes1 = notes1.rotate
                  count1 = (inc count1) % notes1.length
                  (notes1 = notes1.shuffle) if (count1 == 0)
                end
                if beats2.look
                  note = notes2[0]
                  play note, pan: -0.05, sustain: 0.9, amp: 0.5
                  #midi note, sustain: 0.9
                  notes2 = notes2.rotate
                  count2 = (inc count2) % notes2.length
                  (notes2 = notes2.shuffle) if (count2 == 0)
                end
                if beats3.look
                  note = notes3[0] + 12
                  play note, pan: 0.05, sustain: 0.9, amp: 0.5
                  #midi note, sustain: 0.9
                  notes3 = notes3.rotate
                  count3 = (inc count3) % notes3.length
                  (notes3 = notes3.shuffle) if (count3 == 0)
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
  end
end

uncomment do
  in_thread do
    with_random_seed 231122 do
      with_random_source :perlin do
        with_synth :fm do
          notes1 = (range 36, 59) + (range 58, 37)
          beats1 = (spread (notes1.length / 3).round, notes1.length).rotate
          play (hz_to_midi 440), amp: 0.5
          midi (hz_to_midi 440)
          sleep 10
          count1 = 0
          with_bpm 200 do
            (notes1.length / 3).round.times do
              notes1.length.times do
                tick
                if beats1.look
                  note = notes1[0]
                  play note, pan: 0.1, sustain: 1.8, amp: 0.5
                  #midi note - 24, sustain: 1.8
                  notes1 = notes1.rotate
                  count1 = (inc count1) % notes1.length
                  (notes1 = notes1.shuffle) if (count1 == 0)
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
  end
end