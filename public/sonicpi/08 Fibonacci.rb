# Copyright 2023 Kirk Rader. All rights reserved.

# Fibonacci

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

bpm = 180

define :fibonacci do |n|
  if n < 2
    return 1
  else
    return (fibonacci (n - 1)) + (fibonacci (n - 2))
  end
end

comment do
  n = 0
  8.times do
    puts (fibonacci n)
    n = inc n
  end
end

uncomment do
  in_thread do
    #with_random_seed 12860 do
    with_random_seed 102664 do
      with_random_source :perlin do
        with_bpm bpm do
          beats = nil
          notes1 = nil
          notes2 = nil
          comment do
            beats = (spread 4, 6) + (spread 4, 6).rotate
            notes1 = ((range 0, 11).map {|n| 23 + (fibonacci n)}).ring
            notes2 = notes1.reverse
          end
          uncomment do
            beats = (spread 7, 12)
            notes1 = ((range 11, 0).map {|n| 23 + (fibonacci n)}).ring
            notes2 = notes1.reverse
          end
          midi :A4, sustain: 1
          sleep 8
          20.times do
            notes1.length.times do
              tick
              if beats.look
                midi notes1[0], sustain: 2
                midi notes2[0], sustain: 2
                notes1 = notes1.rotate
                notes2 = notes2.rotate
              end
              sleep 1
            end
            notes1 = notes1.shuffle
            notes2 = notes2.shuffle
          end
          midi notes1[0], sustain: 2
          midi notes2[0], sustain: 2
          sleep 1
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end