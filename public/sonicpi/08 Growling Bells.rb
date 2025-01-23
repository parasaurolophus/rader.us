# Copyright 2023 Kirk Rader. All rights reserved.

# 08 Growling Bells

a = 0.75
d = 90
s = 0.2

enable_track_1 = true
enable_track_2 = true
enable_track_3 = true

# track 1
in_thread do
  with_random_seed 3 do
    with_random_source :white do
      with_synth :fm do
        n = play :A3, div_slide: s, divisor: rrand(0.01, 100), sustain: d, amp: a, pan: -0.5 if enable_track_1
        loop do
          stop if get[:terminate]
          control n, divisor: rrand(0.1, 10) if enable_track_1
          sleep rrand(0.2, 2)
        end
      end
    end
  end
end

# track 2
in_thread do
  with_random_seed 5 do
    with_random_source :white do
      with_synth :fm do
        n = play :C4, div_slide: s, divisor: rrand(0.01, 100), sustain: d, amp: a, pan: 0.0 if enable_track_2
        loop do
          stop if get[:terminate]
          control n, divisor: rrand(0.1, 10) if enable_track_2
          sleep rrand(0.2, 2)
        end
      end
    end
  end
end

# track 3
in_thread do
  with_random_seed 7 do
    with_random_source :white do
      with_synth :fm do
        n = play :E4, div_slide: s, divisor: rrand(0.01, 100), sustain: d, amp: a, pan: 0.5 if enable_track_3
        loop do
          stop if get[:terminate]
          control n, divisor: rrand(0.1, 10) if enable_track_3
          sleep rrand(0.2, 2)
        end
      end
    end
  end
end

in_thread do
  set :terminate, false
  d.times do
    cue :master
    sleep 1
  end
ensure
  set :terminate, true
  cue :master
  sleep 3
  set :terminate, false
end
