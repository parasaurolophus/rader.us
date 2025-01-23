# Copyright 2023 Kirk Rader. All rights reserved.

# Shuffle Off To

tempo = 0.125
cues_per_beat = 4
beats_per_measure = 4
measures_per_phrase = 4
cues_per_measure = cues_per_beat * beats_per_measure
cues_per_phrase = cues_per_measure * measures_per_phrase
phrase_loops = 10
total_cues = cues_per_phrase * phrase_loops
measure_duration = tempo * cues_per_measure
room = 0.75

enable_midi = true
enable_synths = true
enable_lead = true
enable_drums = true

terminate = false

define :play_note do |s, n, a, p|
  if enable_midi
    midi n, sustain: tempo * 0.9
  end
  if enable_synths
    synth s, note: n, amp: a, pan: p, sustain: tempo
  end
end

if enable_lead
  in_thread do
    with_random_seed 0 do
      with_random_source :perlin do
        with_fx :reverb, room: room do
          spr = (spread beats_per_measure / 2 + 1, beats_per_measure)
          min_notes = 36
          max_notes = 72
          notes_step = 5
          notes = (range min_notes, max_notes, step: notes_step, inclusive: false)
          notes = notes + (range max_notes, min_notes, step: -notes_step, inclusive: false)
          min_amp = 0.1
          max_amp = 0.5
          amps = (range min_amp, max_amp, inclusive: true, step: ((max_amp - min_amp) / phrase_loops))
          current_phrase = 0
          loop do
            sync :master
            stop if terminate
            tick
            phrase_count = get[:phrase_count]
            if current_phrase != phrase_count
              current_phrase = phrase_count
              notes = notes.shuffle
            end
            if spr.look
              play_note :beep, notes[get[:cue_count]], amps[phrase_count], 0.0
            end
          end
        end
      end
    end
  end
end

if enable_drums
  in_thread do
    with_random_seed 2 do
      bass_spr = (spread 2, cues_per_measure)
      lo_tom_spr = bass_spr.shuffle
      mid_tom_spr = lo_tom_spr.shuffle
      hi_tom_spr = bass_spr.rotate(-cues_per_beat)
      cowbell_spr = (spread cues_per_beat, cues_per_measure).rotate(cues_per_beat / 2)
      cymbal_spr = (spread cues_per_beat * 2, cues_per_measure)
      splash_spr = (spread 1, cues_per_measure * 2)
      with_fx :reverb, room: room do
        loop do
          sync :master
          stop if terminate
          tick
          phrase_count = get[:phrase_count]
          sample :drum_bass_soft, rate: (rrand 0.9, 1.1), pan: -0.5 if bass_spr.look
          sample :drum_tom_lo_soft, rate: (rrand 0.9, 1.1), pan: 0.5 if lo_tom_spr.look && phrase_count > 0
          sample :drum_tom_mid_soft, rate: (rrand 0.9, 1.1), pan: -0.25 if mid_tom_spr.look && phrase_count > 1
          sample :drum_tom_hi_soft, rate: (rrand 0.9, 1.1), pan: 0.25 if hi_tom_spr.look && phrase_count > 2
          sample :drum_cowbell, rate: (rrand 0.9, 1.1), pan: -0.75 if cowbell_spr.look && phrase_count > 3
          sample :drum_cymbal_soft, rate: (rrand 0.9, 1.1), pan: 0.75 if cymbal_spr.look && phrase_count > 4
          sample :drum_splash_hard, rate: (rrand 0.9, 1.1), pan: 0.0 if splash_spr.look && phrase_count > 5
        end
      end
    end
  end
end

begin
  play_note :beep, :A4, 1.0, 0.0
  sleep measure_duration
  cue_count = 0
  beat_count = 0
  measure_count = 0
  phrase_count = 0
  total_cues.times do
    set :cue_count, cue_count
    set :beat_count, beat_count
    set :measure_count, measure_count
    set :phrase_count, phrase_count
    puts cue_count
    puts beat_count
    puts measure_count
    puts phrase_count
    cue :master
    cue_count = inc cue_count
    beat_count = inc beat_count if cue_count % cues_per_beat == 0
    measure_count = inc measure_count if cue_count % cues_per_measure == 0
    phrase_count = inc phrase_count if cue_count % cues_per_phrase == 0
    sleep tempo
  end
ensure
  midi_all_notes_off
  terminate = true
  cue :master
end
