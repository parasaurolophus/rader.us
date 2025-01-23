# Copyright 2023 Kirk Rader. All rights reserved.

# I Alone Am Left

in_thread do
  with_random_seed 3 do
    with_random_source :white do
      loop do
        note = rrand(24, 84)
        #note = rrand(43, 55)
        midi_note_on note
        sleep rrand(1, 5)
        midi_note_off note
        sleep rrand(1, 5)
      end
    end
  end
ensure
  midi_all_notes_off
end
