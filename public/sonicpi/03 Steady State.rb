# Copyright 2023 Kirk Rader. All rights reserved.

# Steady State

comment do
  # tune midi synth
  loop do
    midi :A4, sustain: 3
    synth :fm, note: :A4, sustain: 3
    sleep 3
  end
ensure
  midi_all_notes_off
  sleep 1
end

define :play_note do |pitch, sustain, play_midi, play_synth|
  with_cent_tuning rrand(-50, 50) do
    midi pitch, sustain: sustain if play_midi
    play pitch, sustain: sustain if play_synth
  end
end

with_random_source :white do
  with_random_seed 0 do
    with_synth :fm do
      with_bpm 180 do
        notes1 = (note_range :A2, :A5, pitches: (chord :A, :minor))
        notes2 = (note_range :C3, :C6, pitches: (chord :C, :major))
        notes3 = (note_range :E3, :E6, pitches: (chord :E, :minor))
        notes4 = (note_range :G6, :G3, pitches: (chord :G, :major))
        beats1 = (spread 9, 16)
        beats2 = (spread 4, 7)
        beats3 = (spread 11, 16).rotate
        beats4 = (spread 7, 16).rotate(2)
        270.times do
          tick
          (play_note notes1.look, 0.95, false, false) if beats1.look
          (play_note notes2.look, 0.95, false, false) if beats2.look
          (play_note notes3.look, 0.95, false, false) if beats3.look
          (play_note notes4.look, 0.95, true, false) if beats4.look
          sleep 1
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end
