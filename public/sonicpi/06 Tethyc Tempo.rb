# Copyright 2023 Kirk Rader. All rights reserved.

# Tethyc Tempo

terminate = false

define :drum_thread do |sample_name, spread_hits, spread_beats, rotations, start_phrase|
  in_thread do
    with_random_seed 0 do
      hits = (spread spread_hits, spread_beats)
      rotations.times do
        hits = hits.rotate
      end
      loop do
        sync :master
        stop if terminate
        count = tick
        phrase_count = get[:phrase_count]
        sample sample_name, rate: rrand(0.9, 1.1), amp: rrand(0.9, 1.0) if ((phrase_count >= start_phrase) && hits.look)
      end
    end
  end
end

define :make_ring do |min, max, step, fn|
  r = (ring fn[min])
  n = (min + step)
  count = ((max - n) / step)
  count.times do
    r = (r + (ring fn[n]))
    n = (n + step)
  end
  return r
end

sample :drum_cymbal_pedal if false
(drum_thread :drum_heavy_kick, 5, 7, 0, 0)
(drum_thread :drum_snare_hard, 3, 5, 2, 2)
(drum_thread :drum_splash_hard, 1, 10, 0, 4)
(drum_thread :drum_cymbal_open, 1, 2, 1, 1)

uncomment do
  in_thread do
    begin
      with_random_seed 0 do
        with_random_source :perlin do
          beats = (spread 5, 11)
          notes = (scale :a2, :aeolian)
          notes = notes + (scale :c2, :aeolian)
          notes = notes + (scale :e2, :aeolian)
          notes = notes + (scale :a2, :dorian)
          notes = notes + (scale :c2, :dorian)
          notes = notes + (scale :e2, :dorian)
          notes = notes + (scale :a2, :ionian)
          notes = notes + (scale :c2, :ionian)
          notes = notes + (scale :e2, :ionian)
          notes = notes + (scale :a2, :locrian)
          notes = notes + (scale :c2, :locrian)
          notes = notes + (scale :e2, :locrian)
          loop do
            sync :master
            stop if terminate
            count = tick
            midi notes.look, sustain: (get[:duration] * 0.9) if beats.look
          end
        end
      end
    ensure
      midi_all_notes_off
    end
  end
end

in_thread do
  begin
    with_random_seed 0 do
      tempo = make_ring(0.1, 0.9, 0.1, lambda {|x| 0.1 + Math.sin(x)})
      phrase_length = tempo.length
      (set :phrase_length, phrase_length)
      (set :phrase_count, -1)
      midi :a4
      synth :sine, note: :a4
      sleep 4
      (phrase_length * 20).times do
        count = tick
        (set :phrase_count, get[:phrase_count] + 1) if ((count % phrase_length) == 0)
        set :duration, tempo.look
        cue :master
        sleep get[:duration]
      end
    end
  ensure
    terminate = true
    set :phrase_count, -1
    cue :master
    sleep 0.5
    cue :master
    sleep 0.5
    midi_all_notes_off
  end
end
