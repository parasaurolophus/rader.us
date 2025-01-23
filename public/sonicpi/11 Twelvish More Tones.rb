# Copyright 2023 Kirk Rader. All rights reserved.

# Twelvish More Tones

terminate = false
bpm = 140
sustain = ((60.0 / bpm) * 0.95)

define :play_tone do |tone, pan, m, s|
  with_fx :reverb, room: 0.6 do
    with_fx :ring_mod, freq: (tone / 2.0), mix: 0.5 do
      with_synth :fm do
        with_bpm bpm do
          d = rrand(0.1, 10.0)
          if m
            midi_note_on tone
          end
          if s
            play tone, divisor: d, amp: 0.5, sustain: sustain, pan: pan
          end
        end
      end
    end
  end
end

define :play_tones do |seed, min, octaves, repeats, pan, m, s|
  with_random_seed seed do
    notes = (range min, min + (12 * octaves)).repeat(repeats).shuffle
    in_thread do
      beats = (spread (rrand_i 1, (notes.length - 1)), notes.length).shuffle
      loop do
        sync :master
        midi_all_notes_off
        tick
        break if terminate
        if beats.look
          (play_tone notes[0], pan, m, s)
          notes = notes.rotate
        end
      end
    end
    notes.length
  end
end

uncomment do
  in_thread do
    with_bpm bpm do
      length1 = (play_tones 17, :A3, 3, 1, 0.5, false, true)
      length2 = (play_tones 161, :C4, 2, 2, 0.0, false, true)
      length3 = (play_tones 593, :E4, 1, 3, -0.5, false, true)
      duration = ((length1 + length2 + length3) * 2)
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
