# Copyright 2023 Kirk Rader. All rights reserved.

# March of the Airy Fairies

comment do
  # tune midi synth
  loop do
    midi :A4, sustain: 0.99
    synth :fm, note: :A4, sustain: 0.99
    sleep 1
  end
ensure
  midi_all_notes_off
end

define :play_sample do |s, p|
  sample s, amp: rrand(0.9, 1.0), pan: p, rate: rrand(0.99, 1.1)
end

comment do
  in_thread do
    with_bpm 200 do
      beats = (spread 3, 8) + (ring false).repeat(8)
      (synth :sine, note: :A4)
      sleep 16
      600.times do
        tick
        (sample :drum_tom_lo_hard, amp: (rrand 0.9, 1.0), rate: (rrand 0.9, 1.1), pan: -0.5) if beats.look
        sleep 1
      end
    end
  end
end

comment do
  in_thread do
    with_bpm 200 do
      beats = (spread 4, 7).rotate + (ring false).repeat(7)
      (synth :sine, note: :A4)
      sleep 16
      600.times do
        tick
        (sample :drum_tom_mid_hard, amp: (rrand 0.9, 1.0), rate: (rrand 0.9, 1.1), pan: 0.0) if beats.look
        sleep 1
      end
    end
  end
end

comment do
  in_thread do
    with_bpm 200 do
      beats = (ring false).repeat(8) + (spread 5, 8)
      (synth :sine, note: :A4)
      sleep 16
      600.times do
        tick
        (sample :drum_tom_hi_hard, amp: (rrand 0.9, 1.0), rate: (rrand 0.9, 1.1), pan: 0.5) if beats.look
        sleep 1
      end
    end
  end
end

comment do
  in_thread do
    with_bpm 200 do
      beats = (spread 9, 16)
      (synth :sine, note: :A4)
      sleep 16
      600.times do
        tick
        (sample :drum_cowbell, amp: (rrand 0.9, 1.0), rate: (rrand 0.9, 1.1), pan: 0.5) if beats.look
        sleep 1
      end
    end
  end
end

comment do
  in_thread do
    with_random_seed 0 do
      with_random_source :pink do
        with_bpm 200 do
          beats = (spread 3, 8) + (spread 5, 8).rotate
          midi :A4
          sleep 16
          600.times do
            tick
            (midi (rrand_i 24, 84), sustain: 0.99) if beats.look
            sleep 1
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end

comment do
  in_thread do
    with_random_seed 42 do
      with_random_source :pink do
        with_bpm 200 do
          beats = (spread 4, 7) + (spread 3, 7).rotate
          midi :A4
          sleep 16
          600.times do
            tick
            (midi (rrand_i 24, 84), sustain: 0.99) if beats.look
            sleep 1
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end

comment do
  in_thread do
    with_random_seed 365 do
      with_random_source :pink do
        with_bpm 200 do
          beats = (spread 7, 16).rotate + (spread 9, 16)
          midi :A4
          sleep 16
          600.times do
            tick
            (midi (rrand_i 24, 84), sustain: 0.99) if beats.look
            sleep 1
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end

comment do
  in_thread do
    with_random_seed 0 do
      with_random_source :pink do
        with_bpm 200 do
          beats = (spread 3, 7).rotate + (spread 3, 8)
          midi :A4
          sleep 16
          600.times do
            tick
            (midi (rrand_i 24, 84), sustain: 0.99) if beats.look
            sleep 1
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end

uncomment do
  in_thread do
    with_random_seed 1023 do
      with_random_source :pink do
        with_bpm 200 do
          beats = (spread 9, 16).rotate
          midi :A4
          sleep 16
          600.times do
            tick
            (midi (rrand_i 24, 84), sustain: 0.99) if beats.look
            sleep 1
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end
