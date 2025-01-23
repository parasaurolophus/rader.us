# Copyright 2023 Kirk Rader. All rights reserved.

# Drunken Calliope

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
          notes3 = nil
          notes4 = nil
          comment do
            notes1 = (note_range :C1, :C3, pitches: (chord :C, :major))
            notes1 = notes1 + (note_range :F1, :F3, pitches: (chord :F, :major))
            notes1 = notes1 + (note_range :G1, :G3, pitches: (chord :G, :major))
            notes2 = (note_range :C6, :C4, pitches: (chord :C, :major))
            notes2 = notes2 + (note_range :F6, :F4, pitches: (chord :F, :major))
            notes2 = notes2 + (note_range :G6, :G4, pitches: (chord :G, :major))
            notes3 = (note_range :C1, :C3, pitches: (chord :C, :major))
            notes4 = (note_range :C6, :C4, pitches: (chord :C, :major))
          end
          uncomment do
            notes1 = (note_range :C3, :C1, pitches: (chord :C, :major))
            notes1 = notes1 + (note_range :F3, :F1, pitches: (chord :F, :major))
            notes1 = notes1 + (note_range :G3, :G1, pitches: (chord :G, :major))
            notes2 = (note_range :C4, :C6, pitches: (chord :C, :major))
            notes2 = notes2 + (note_range :F4, :F6, pitches: (chord :F, :major))
            notes2 = notes2 + (note_range :G4, :G6, pitches: (chord :G, :major))
            notes3 = (note_range :C3, :C1, pitches: (chord :C, :major))
            notes4 = (note_range :C4, :C6, pitches: (chord :C, :major))
          end
          notes1 = notes1.take(notes1.length - 1)
          notes2 = notes2.drop(1)
          midi :A4, sustain: 1
          sleep 4
          count = 0
          15.times do
            n = (inc (count % 8))
            beats = nil
            comment do
              beats = (spread n, 8)
            end
            uncomment do
              n = (8.0 / n).floor
              beats = (spread n, 8)
            end
            sustain = (1.0 + (8.0 / n))
            count = (inc count)
            notes1.length.times do
              if beats.tick
                midi notes1.look, sustain: sustain
                midi notes2.look, sustain: sustain
              end
              sleep 1
            end
          end
          (notes3.length - 1).times do
            tick
            midi notes3.look, sustain: 2
            midi notes4.look, sustain: 2
            sleep 1
          end
          midi notes3[notes3.length - 1], sustain: 8
          midi notes4[notes4.length - 1], sustain: 8
          sleep 9
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end
