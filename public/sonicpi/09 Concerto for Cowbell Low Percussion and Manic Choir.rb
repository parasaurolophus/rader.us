# Copyright Kirk Rader 2023. All rights reserved.

# Concerto for Cowbell, Low Percussion and Manic Choir

terminate = false

define :choir do |trigger, min, max|
  in_thread do
    with_random_source :perlin do
      with_sample_bpm :ambi_choir do
        loop do
          sync trigger
          if terminate
            stop
          end
          rate = rrand(min, max)
          sample :ambi_choir, rate: rate
          sleep rate
        end
      end
    end
  end
end

choir :choir_lo, 0.25, 0.75
choir :choir_hi, 1, 1.5

in_thread do
  with_fx :reverb do
    tick_spread = (spread 1, 1)
    bass_spread = (spread 1, 16)
    tom_lo_spread_odd = (spread 3, 16)
    tom_lo_spread_even = tom_lo_spread_odd
    tom_hi_spread = (spread 5, 16)
    cowbell_spread_odd = (spread 11, 16)
    cowbell_spread_even = cowbell_spread_odd
    cowbell_amp = (ring 1, 0.25, 0.75)
    4.times do
      tom_lo_spread_odd = tom_lo_spread_odd.rotate
      tom_hi_spread = tom_hi_spread.rotate
      cowbell_spread_even = cowbell_spread_even.rotate
    end
    count = 0
    256.times do
      tick_spread.tick
      choir_cue = (count % 16)
      if choir_cue < 8
        cue :choir_lo
      else
        cue :choir_hi
      end
      even = (choir_cue == 0)
      sample :drum_bass_hard if bass_spread.look
      if even
        sample :drum_tom_lo_soft if tom_lo_spread_even.look
        sample :drum_tom_hi_hard if tom_hi_spread.look
        sample :drum_cowbell, amp: cowbell_amp.look if cowbell_spread_even.look
      else
        sample :drum_tom_lo_hard if tom_lo_spread_odd.look
        sample :drum_tom_hi_soft if tom_hi_spread.look
        sample :drum_cowbell, amp: cowbell_amp.look if cowbell_spread_odd.look
      end
      count = count + 1
      sleep 0.125
    end
    terminate = true
    sleep 5
    cue :choir_lo
    cue :choir_hi
  end
end
