# Copyright 2023 Kirk Rader. All rights reserved.

# YAMP

in_thread do
  with_random_seed 1 do
    with_random_source :pink do
      beats = (spread 7, 16)
      #beats = (spread 11, 16).rotate
      #beats = (spread 15, 16)
      #beats = (spread 3, 16)
      midi :A4
      sleep 4.0
      360.times do
        tick
        note = rrand_i(24, 55)
        #note = rrand_i(43, 84)
        #note = rrand_i(43, 55)
        #note = rrand_i(24, 36)
        midi_all_notes_off if beats.look
        sleep 0.05
        midi_note_on note if beats.look
        sleep 0.2
      end
    end
  end
ensure
  midi_all_notes_off
end
