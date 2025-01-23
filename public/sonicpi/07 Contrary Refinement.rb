# Copyright 2023 Kirk Rader. All rights reserved.

# Contrary Refinement

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
    #with_random_seed 0 do
    #with_random_seed 471 do
    with_random_seed 12860 do
      with_random_source :perlin do
        with_bpm bpm do
          beats = nil
          notes1 = nil
          notes2 = nil
          comment do
            beats = (spread 5, 8)
            notes1 = (note_range :C3, :B4).reflect
            notes2 = (note_range :B5, :C4).reflect
          end
          uncomment do
            beats = (spread 4, 7)
            notes1 = (note_range :A3, :Gs5).reflect
            notes2 = (note_range :Gs6, :A4).reflect
          end
          comment do
            beats = (spread 3, 8).rotate
            notes1 = (note_range :F2, :E4).reflect
            notes2 = (note_range :E3, :F5).reflect
          end
          midi :A4, sustain: 1
          sleep 8
          5.times do
            notes1.length.times do
              tick
              if beats.look
                midi notes1[0], sustain: 2
                midi notes2[0], sustain: 2
                notes1 = notes1.rotate
                notes2 = notes2.rotate
              end
              sleep 1
            end
            notes1 = notes1.shuffle
            notes2 = notes2.shuffle
          end
          midi notes1[0], sustain: 2
          midi notes2[0], sustain: 2
          sleep 1
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end