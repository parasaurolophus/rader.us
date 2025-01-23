# Copyright 2023 Kirk Rader. All rights reserved.

# Tower of Sines

in_thread do
  with_random_seed 1 do
    with_random_source :pink do
      notes = (range -Math::PI, Math::PI, ((2 * Math::PI) / 36.0))
      notes = notes.map { |n| (53.0 + (36.0 * Math.sin(n))).floor }
      #beats = (spread 7, 16)
      #beats = (spread 5, 16).rotate
      beats = (spread 3, 16)
      #beats = (spread 4, 7)
      midi :A4
      sleep 4.0
      (notes.length * 10).times do
        tick
        midi_all_notes_off if beats.look
        sleep 0.1
        if beats.look
          note = notes[0]
          notes = notes.rotate
          midi_note_on note
        end
        sleep 0.2
      end
    end
  end
ensure
  midi_all_notes_off
end
