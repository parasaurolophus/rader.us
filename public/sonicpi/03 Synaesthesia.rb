# Copyright 2023 Kirk Rader. All rights reserved.

# Synaesthesia

enable_midi_1 = false
enable_midi_2 = true
enable_drums = true
bpm = 16
tempo = 0.2
phrase_length = bpm * 3
loops = 10

terminate = false
count = 0
phrase_count = 0

define :drum_hit do |samp, pan|
  with_sample_bpm samp do
    rate = rrand(0.9, 1.1)
    sample samp, rate: rate, sustain: rate, pan: pan
  end
end

in_thread do
  with_random_seed 0 do
    with_random_source :pink do
      synth1_spr = (spread 5,bpm / 2 - 1)
      synth2_spr = synth1_spr.shuffle
      notes1 = (range 24, 48, inclusive: true)
      notes2 = (range 48, 24, step: -1, inclusive: true)
      drum1_spr = (spread 1, bpm)
      drum2_spr = (spread 3, bpm)
      drum3_spr = (spread 4, bpm / 2 - 1)
      drum4_spr = (spread 1, bpm / 2)
      drum5_spr = (spread 3, bpm / 2 + 1)
      drum6_spr = (spread bpm, bpm)
      loop do
        sync :master
        midi_all_notes_off
        stop if terminate
        tick
        if enable_midi_1 && phrase_count > 6 && synth1_spr.look
          midi_note_on (rrand_i 48, 60) if enable_midi_1
        end
        if enable_midi_2 && phrase_count > 5 && synth2_spr.look
          if count % 2 == 0
            note = notes1[count]
          else
            note = notes2[count]
          end
          midi_note_on note
        end
        if enable_drums
          (drum_hit :drum_bass_soft, 0.5) if phrase_count > 0 && drum1_spr.look
          (drum_hit :drum_tom_lo_soft, -0.25) if phrase_count > 1 && drum2_spr.look
          (drum_hit :drum_tom_mid_soft, 0.25) if phrase_count > 2 && drum3_spr.look
          (drum_hit :drum_tom_hi_soft, -0.5) if phrase_count > 3 && drum4_spr.look
          (drum_hit :drum_snare_soft, 0.0) if phrase_count > 4 && drum5_spr.look
          (drum_hit :drum_cymbal_pedal, 0.0) if drum6_spr.look
        end
      end
    end
  end
end

begin
  midi :A4, sustain: 1
  synth :beep, note: :A4
  sleep 3
  (phrase_length * loops).times do
    puts count
    puts phrase_count
    cue :master
    count = inc count
    phrase_count = inc phrase_count if ((count % phrase_length) == 0)
    sleep tempo
  end
ensure
  midi_all_notes_off
  terminate = true
  cue :master
end
