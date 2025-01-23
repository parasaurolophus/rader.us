# Copyright 2023 Kirk Rader. All rights reserved.

# Contrary Trigonometry

define :make_notes do |start, stop, length, base, fn|
  step = ((stop - start) / length)
  notes = (range start, stop, step)
  (notes.map { |x| (base + (fn[x] * length)).round }).ring
end

uncomment do
  in_thread do
    with_random_seed 0 do
      with_random_source :white do
        beats = (spread 1, 4)
        notes = (range 24, 84, 1)
        repeats = 10
        comment do
          # track 1
          beats = (spread 11, 16)
          notes = (make_notes -1.0, 1.0, 12, 41, lambda { |x| Math.tan(x) }).repeat
          repeats = (notes.length * 10)
        end
        comment do
          # track 2
          beats = (spread 4, 7)
          notes = (make_notes 1.0, -1.0, 24, 41, lambda { |x| Math.tan(x) })
          repeats = (notes.length * 10)
        end
        uncomment do
          # track 3
          beats = (spread 7, 16).rotate
          notes = (make_notes Math::PI, -Math::PI, 24, 53, lambda { |x| Math.sin(x) })
          repeats = (notes.length * 10) - 4
        end
        midi :A4
        sleep 4
        repeats.times do
          tick
          midi_all_notes_off if beats.look
          sleep 0.1
          if beats.look
            midi_note_on notes.look
            #notes = notes.rotate
          end
          sleep 0.2
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end
