# Copyright Kirk Rader 2023. All rights reserved.

# Sinfonietta for Cowbell Orchestra

terminate = false

define :cow_thread do |trigger, rate|
  in_thread do
    with_random_source :perlin do
      with_random_source :perlin do
        drift = rate * 0.1
        loop do
          sync trigger
          if terminate
            stop
          end
          r = rate + rrand(-drift, drift)
          sample :drum_cowbell, rate: r, pan: rrand(-0.5, 0.5)
        end
      end
    end
  end
end

define :rand_cow_thread do |trigger, min, max|
  in_thread do
    with_random_source :perlin do
      loop do
        sync trigger
        if terminate
          stop
        end
        sample :drum_cowbell, rate: rrand(min, max), pan: rrand(-0.5, 0.5)
      end
    end
  end
end

cow_thread :low, 0.25
cow_thread :mid, 1
cow_thread :hi, 1.75
rand_cow_thread :rand, 0.5, 1.5

lo_spread = (spread 3, 8)
mid_spread = lo_spread.rotate
hi_spread = (spread 5, 8)
rand_spread = (spread 4, 7)

sleeps = (ring 0.66, 0.33, 1, 0.5)

with_sample_bpm :drum_cowbell do
  sleeps.length.times do
    s = sleeps.tick
    100.times do
      cue :hi if hi_spread.tick
      cue :mid if mid_spread.look
      cue :low if lo_spread.look
      cue :rand if rand_spread.look
      sleep s
    end
  end
end

terminate = true
cue :low
cue :mid
cue :hi
cue :rand