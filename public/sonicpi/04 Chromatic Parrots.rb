# Copyright 2023 Kirk Rader. All rights reserved.

# Chromatic Parrots

tempo = 0.2
#beats_per_measure = 8
#beats_per_measure = 12
beats_per_measure = 16
#measures_per_phrase = 4
#measures_per_phrase = 3
measures_per_phrase = 2
beats_per_phrase = beats_per_measure * measures_per_phrase
phrase_loops = 10
total_beats = beats_per_phrase * phrase_loops
room = 0.75

enable_midi = false
enable_lead = true
enable_bass = true
enable_drone = true

terminate = false
beat_count = 0
phrase_count = 0

define :play_note do |s, n, p|
  if enable_midi
    midi n, sustain: tempo * 0.9
  else
    synth s, note: n, pan: p, sustain: tempo
  end
end

if enable_lead
  in_thread do
    with_fx :reverb, room: room do
      spr = (spread beats_per_measure / 2 + 1, beats_per_measure)
      notes1 = (range 36, 60, inclusive: true)
      notes2 = (range 60, 36, inclusive: true, step: -1)
      loop do
        sync :master
        stop if terminate
        tick
        if phrase_count > 3 && spr.look
          if beat_count % 2 == 0
            n = notes1[0]
            notes1 = notes1.rotate
          else
            n = notes2[0]
            notes2 = notes2.rotate
          end
          synth :pretty_bell if false
          (play_note :pretty_bell, n, 0.5)
        end
      end
    end
  end
end

if enable_bass
  in_thread do
    with_fx :reverb, room: room do
      spr = (spread beats_per_measure / 2 - 1, beats_per_measure)
      notes1 = (range 24, 48, inclusive: false)
      notes2 = (range 48, 24, inclusive: false, step: -1)
      notes = notes1 + notes2
      loop do
        sync :master
        stop if terminate
        tick
        if phrase_count > 1 && spr.look
          synth :bass_foundation if false
          (play_note :bass_foundation, notes[0], -0.5)
          notes = notes.rotate
        end
      end
    end
  end
end

if enable_drone
  in_thread do
    with_fx :reverb, room: room do
      hits_per_measure = beats_per_measure / 4
      sustain = tempo * (beats_per_measure / hits_per_measure)
      spr = (spread hits_per_measure, beats_per_measure)
      rate_min = (hits_per_measure * 1.0) / beats_per_measure
      rates = (range rate_min, 1.0, step: rate_min, inclusive: true)
      loop do
        sync :master
        stop if terminate
        tick
        sample :ambi_drone if false
        s = :ambi_drone
        with_sample_bpm s do
          if spr.look
            (sample s, rate: rates[0], sustain: sustain)
            rates = rates.rotate
          end
        end
      end
    end
  end
end

begin
  total_beats.times do
    puts total_beats
    puts beat_count
    puts phrase_count
    cue :master
    beat_count = inc beat_count
    phrase_count = inc phrase_count if beat_count % beats_per_phrase == 0
    sleep tempo
  end
ensure
  midi_all_notes_off
  terminate = true
  cue :master
end
