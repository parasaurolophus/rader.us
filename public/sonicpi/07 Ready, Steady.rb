# Copyright 2023 Kirk Rader. All rights reserved.

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

define :play_note do |pitch, sustain, pan, play_midi, play_synth|
  with_cent_tuning rrand_i(-10, 10) do
    (midi pitch, sustain: sustain) if play_midi
    (play pitch, sustain: sustain, pan: pan, amp: 0.33) if play_synth
  end
end

uncomment do
  with_random_source :white do
    with_random_seed 0 do
      with_synth :fm do
        with_fx :reverb, mix: 0.5, room: 0.5, mix_slide: 0.1, room_slide: 0.1 do |reverb|
          with_bpm 200 do
            notes1 = (note_range :A2, :A5, pitches: (chord :A, :major)).shuffle
            notes2 = (note_range :A2, :A5, pitches: (chord :A, :major)).shuffle
            notes3 = (note_range :D3, :D6, pitches: (chord :D, :major)).shuffle
            notes4 = (note_range :E3, :E6, pitches: (chord :E, :major)).shuffle
            notes5 = (note_range :A5, :A2, pitches: (chord :A, :major))
            notes6 = (note_range :A2, :A5, pitches: (chord :A, :major))
            notes7 = (note_range :D6, :D3, pitches: (chord :D, :major))
            notes8 = (note_range :E6, :E3, pitches: (chord :E, :major))
            beats1 = (spread 15, 16)
            beats2 = (spread 6, 7)
            beats3 = (spread 9, 16).rotate
            beats4 = (spread 9, 16).rotate(2)
            play_odd_midi = false
            play_even_midi = !play_odd_midi
            play_synth = true
            uncomment do
              (20 * notes1.length).times do
                tick
                control reverb, room: rrand(0.1, 1.0)
                control reverb, mix: rrand(0.1, 1.0)
                (play_note notes1.look, 0.99, 0.5, play_odd_midi, play_synth) if beats1.look
                (play_note notes2.look, 0.99, 0.5, play_even_midi, play_synth) if beats2.look
                (play_note notes3.look, 0.99, -0.25, play_odd_midi, play_synth) if beats3.look
                (play_note notes4.look, 0.99, -0.25, play_even_midi, play_synth) if beats4.look
                sleep 1
              end
            end
            notes5.length.times do
              tick
              control reverb, room: rrand(0.1, 1.0)
              control reverb, mix: rrand(0.1, 1.0)
              (play_note notes5.look, 0.99, 0.5, play_odd_midi, play_synth) if beats1.look
              (play_note notes6.look, 0.99, 0.5, play_even_midi, play_synth) if beats2.look
              (play_note notes7.look, 0.99, -0.25, play_odd_midi, play_synth) if beats3.look
              (play_note notes8.look, 0.99, -0.25, play_even_midi, play_synth) if beats4.look
              sleep 1
            end
            tick
            control reverb, room: rrand(0.1, 1.0)
            control reverb, mix: rrand(0.1, 1.0)
            (play_note :A2, 3.99, 0.5, play_odd_midi, play_synth)
            (play_note :A5, 3.99, 0.5, play_even_midi, play_synth)
            (play_note :Cs4, 3.99, -0.25, play_odd_midi, play_synth)
            (play_note :E5, 3.99, -0.25, play_even_midi, play_synth)
            sleep 4
          ensure
            midi_all_notes_off
          end
        end
      end
    end
  end
end
