# Copyright 2023 Kirk Rader. All rights reserved.

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
          melody_beats = (spread 5, 7)
          kick_beats = (spread 1, 8).rotate(7)
          lo_tom_beats = (spread 3, 8)
          notes1 = (note_range :C2, :B3).reflect.shuffle
          notes2 = (note_range :B5, :C4).reflect
          sample :drum_bass_hard, amp: 0.5
          midi :A4, sustain: 1
          sleep 4
          count = 0
          10.times do
            notes1.length.times do
              tick
              (sample :drum_heavy_kick, rate: (rrand 0.9, 1.1), amp: 0.5) if kick_beats.look
              (sample :drum_tom_lo_hard, rate: (rrand 0.9, 1.1), amp: 0.5) if lo_tom_beats.look
              if melody_beats.look
                midi notes1[0], sustain: 2
                midi notes2[0], sustain: 2
                notes1 = notes1.rotate
                notes2 = notes2.rotate
              end
              sleep 1
            end
          end
          sleep 9
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end
