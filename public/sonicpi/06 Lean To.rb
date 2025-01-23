# Copyright 2023 Kirk Rader. All rights reserved.

# Lean To

define :make_notes do |start, stop, length, base, fn|
  step = ((stop - start) / length)
  notes = (range start, stop, step)
  (notes.map { |x| (base + (fn[x] * length)).round }).ring
end

uncomment do
  in_thread do
    with_random_seed 0 do
      with_random_source :white do
        beats = false
        notes = false
        uncomment do
          # track 1
          beats = (spread 15, 16)
          notes = (make_notes -1.0, 1.0, 24, 53, lambda { |x| Math.tan(x) })
        end
        comment do
          # track 2
          beats = (spread 15, 16).rotate
          notes = (make_notes -1.0, 1.0, 24, 53, lambda { |x| Math.tan(x) })
        end
        comment do
          # track 3
          beats = (spread 13, 16)
          notes = (make_notes 1.0, -1.0, 24, 53, lambda { |x| Math.tan(x) }).rotate(2)
        end
        comment do
          # track 4
          beats = (spread 13, 16)
          notes = (make_notes 1.0, -1.0, 24, 53, lambda { |x| Math.tan(x) }).rotate(3)
        end
        midi :A4
        sleep 4
        (notes.length * 10).times do
          tick
          midi_all_notes_off if beats.look
          sleep 0.1
          if beats.look
            midi_note_on notes[0]
            notes = notes.rotate
          end
          sleep 0.2
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end
