# Copyright 2023 Kirk Rader. All rights reserved.

# Whistling Gongs

in_thread do
  with_random_seed 3 do
    with_random_source :white do
      midi :A4
      sleep 10.0
      60.times do
        note = rrand_i(24, 84)
        #note = rrand_i(43, 55)
        midi_note_on note
        sleep rrand(1.0, 2.0)
        midi_note_off note
        sleep rrand(1.0, 2.0)
      end
    end
  end
ensure
  midi_all_notes_off
end
