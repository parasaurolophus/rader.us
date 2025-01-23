# Copyright 2023 Kirk Rader. All rights reserved.

# Prime

terminate = false

define :play_notes do |m, v, seed, source, lo, hi, beats, measure, rot|
  with_random_seed seed do
    with_random_source source do
      with_synth v do
        b = (spread beats, measure).rotate(rot)
        loop do
          sync :master
          stop if terminate
          if b.tick
            if m
              midi (rrand_i lo, hi)
            else
              play (rrand_i lo, hi)
            end
          end
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end

in_thread do
  with_bpm 160 do
    play 57
    midi 57
    sleep 10
    319.times do
      cue :master
      sleep 1
    end
    terminate = true
    cue :master
    sleep 1
  end
end

uncomment do
  
  in_thread do
    play_notes false, :fm, 1960, :light_pink, 60, 84, 3, 5, 0
  end
  
  in_thread do
    play_notes false, :fm, 1960, :light_pink, 43, 67, 5, 7, 0
  end
  
  in_thread do
    play_notes false, :fm, 1960, :light_pink, 40, 64, 7, 11, 0
  end
  
  in_thread do
    play_notes false, :fm, 1960, :light_pink, 24, 48, 7, 13, 0
  end
  
end

in_thread do
  with_random_seed 0 do
    with_random_source :white do
      beats1 = (spread 2, 3).rotate(0)
      beats2 = beats1.rotate(1)
      hit = false
      measure = -1
      loop do
        sync :master
        stop if terminate
        tick
        measure = (inc measure) if ((look % beats1.length) == 0)
        if ((measure % 3) == 2)
          puts 2
          hit = beats2.look
        else
          puts 1
          hit = beats1.look
        end
        sample :drum_cymbal_closed, rate: (rrand 0.9, 1.1) if hit
      end
    ensure
      midi_all_notes_off
    end
  end
end

uncomment do
  in_thread do
    with_random_seed 0 do
      with_random_source :white do
        beats = (spread 1, 3).rotate(0)
        loop do
          sync :master
          stop if terminate
          sample :drum_bass_hard, rate: (rrand 0.9, 1.1) if beats.tick
        end
      ensure
        midi_all_notes_off
      end
    end
  end
end
