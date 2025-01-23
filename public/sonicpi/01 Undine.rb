# Copyright 2023 Kirk Rader. All rights reserved.

# Undine

define :make_ring do |min, max, step, fn|
  r = (ring)
  x = min
  while x <= max
    r = r + (ring fn[x])
    x = x + step
  end
  return r
end

define :reduce do |array, a, fn|
  array.each{|x| a = fn[a, x]}
  return a
end

in_thread do
  with_random_seed 0 do
    begin
      pi2 = (Math::PI * 2)
      notes_range = 24
      notes_step = (pi2 / notes_range)
      step = ((Math::PI * 2) / notes_range)
      notes = (make_ring -Math::PI, Math::PI, step, lambda {|x| 40 + (Math.sin(x) * notes_range).round}).reverse
      rates = (make_ring -Math::PI, Math::PI, (pi2 / 16), lambda {|x| 0.3 + (Math.sin(x) * 0.25)})
      rest_duration = (reduce rates, 0, lambda {|a, x| a + x})
      beats = (spread 15, 16)
      synth :sine, note: :A4
      midi :A4
      sleep 4
      1.times do
        (notes.length * 9).times do
          tick
          midi_note_on notes.look if beats.look
          sleep rates.look * 0.9
          midi_note_off notes.look if beats.look
          sleep rates.look * 0.1
        end
        sleep rest_duration
      end
    ensure
      midi_all_notes_off
    end
  end
end
