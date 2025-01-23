# Copyright 2023 Kirk Rader. All rights reserved.

# Tethyc Melody

terminate = false

define :drum_thread do |sample_name, spread_hits, spread_beats, rotations, start_phrase, pan|
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
        (sample sample_name, rate: rrand(0.9, 1.1), amp: rrand(0.9, 1.0), pan: pan) if ((phrase_count >= start_phrase) && hits.look)
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

comment do
  sample :drum_tom_hi_soft if false
  drum_thread :drum_tom_lo_hard, 3, 16, 0, 0, 0.0
  drum_thread :drum_tom_mid_soft, 5, 16, 4, 0, -0.5
  drum_thread :drum_tom_hi_soft, 3, 5, 2, 0, 0.5
end

uncomment do
  in_thread do
    begin
      with_random_seed 0 do
        with_random_source :perlin do
          beats = (spread 5, 11)
          pi_over_2 = (Math::PI / 2.0)
          notes = make_ring(-pi_over_2, pi_over_2, 0.1, lambda {|x| 48 + (12.0 * Math.sin(x)).round})
          notes = notes + make_ring(pi_over_2, -pi_over_2, -0.1, lambda {|x| 48 + (12.0 * Math.sin(x)).round})
          loop do
            sync :master
            stop if terminate
            count = tick
            if beats.look
              midi notes.choose, sustain: (get[:duration] * 0.9)
              #notes = notes.rotate
            end
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
      set :phrase_length, 48
      set :phrase_count, -1
      set :duration, 0.2
      midi :a4
      synth :sine, note: :a4
      sleep 4
      (get[:phrase_length] * 10).times do
        (set :phrase_count, get[:phrase_count] + 1) if ((look % get[:phrase_length]) == 0)
        cue :master
        sleep get[:duration]
      end
    end
  ensure
    terminate = true
    set :phrase_count, -1
    cue :master
  end
end
