# Copyright 2023 Kirk Rader. All rights reserved.

# XII

terminate = false
bpm = 180
sustain = ((60.0 / bpm) * 0.95)

define :pan do |m, n|
  0.5 - (m * (1.0 / n))
end

define :play_tone do |tone, pan, m, s|
  with_synth :fm do
    with_bpm bpm do
      d = rrand(0.1, 10.0)
      if m
        midi tone, sustain: sustain
      end
      if s
        play tone, divisor: d, amp: 0.5, sustain: sustain, pan: pan
      end
    end
  end
end

define :play_tones do |seed, min, octaves, repeats, pan, m, s|
  with_fx :ring_mod, freq: 1.0, freq_slide: 0.01 do |ring_mod|
    with_random_seed seed do
      with_random_source :perlin do
        notes = (range min, min + (12 * octaves)).repeat(repeats).shuffle
        in_thread do
          beats = (spread (rrand_i 1, (notes.length - 1)), notes.length).shuffle
          o = -1
          loop do
            sync :master
            o = ((inc o) % 3) if ((tick % 12) == 0)
            break if terminate
            if beats.look
              with_octave o do
                control ring_mod, freq: (rrand 0.1, 10.0)
                (play_tone notes[0], pan, m, s)
              end
              notes = notes.rotate
            end
          end
        end
        notes.length
      end
    end
  end
end

uncomment do
  in_thread do
    with_random_seed 0 do
      with_random_source :white do
        with_bpm bpm do
          length1 = (play_tones (rrand_i 1, 100), :A1, 1, 12, (pan 0.0, 12.0), true, false)
          length2 = (play_tones (rrand_i 1, 100), :As1, 2, 6, (pan 1.0, 12.0), false, false)
          length3 = (play_tones (rrand_i 1, 100), :B1, 3, 4, (pan 2.0, 12.0), false, false)
          length4 = (play_tones (rrand_i 1, 100), :C2, 1, 12, (pan 3.0, 12.0), false, false)
          length5 = (play_tones (rrand_i 1, 100), :Cs2, 2, 6, (pan 4.0, 12.0), false, false)
          length6 = (play_tones (rrand_i 1, 100), :D2, 3, 4, (pan 5.0, 12.0), false, false)
          length7 = (play_tones (rrand_i 1, 100), :Ds2, 1, 12, (pan 6.0, 12.0), false, false)
          length8 = (play_tones (rrand_i 1, 100), :E2, 2, 6, (pan 7.0, 12.0), false, false)
          length9 = (play_tones (rrand_i 1, 100), :F2, 3, 4, (pan 8.0, 12.0), false, false)
          length10 = (play_tones (rrand_i 1, 100), :Fs2, 1, 12, (pan 9.0, 12.0), false, false)
          length11 = (play_tones (rrand_i 1, 100), :G2, 2, 6, (pan 10.0, 12.0), false, false)
          length12 = (play_tones (rrand_i 1, 100), :Gs2, 3, 4, (pan 11.0, 12.0), false, false)
          duration = 288 # (length1 + length2 + length3 + length4 + length5 + length6 + length7 + length8 + length9 + length10 + length11 + length12)
          play :A4
          midi :A4, sustain: 0.1
          sleep 10
          duration.times do
            cue :master
            sleep 1
          end
        ensure
          terminate = true
          cue :master
          sleep 2
          midi_all_notes_off
        end
      end
    end
  end
end
