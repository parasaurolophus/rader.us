# Copyright 2023 Kirk Rader. All rights reserved.

# Shoenbergian Steampunk

in_thread do
  sync :start
  # track 1
  seed = 17
  source = :white
  note_min = 36
  note_max = 48
  comment do
    # track 2
    seed = 131
    source = :white
    note_min = 41
    note_max = 53
  end
  comment do
    # track 3
    seed = 457
    source = :white
    note_min = 49
    note_max = 61
  end
  with_random_seed seed do
    with_random_source source do
      notes = (range note_min, note_max).shuffle
      loop do
        midi_all_notes_off
        stop if get[:terminate]
        sleep 0.1
        midi_note_on notes.tick
        sleep rrand(0.2, 2)
      end
    end
  end
end

in_thread do
  set :terminate, false
  midi :A4, sustain: 0.1
  synth :beep
  sleep 4
  cue :start
  90.times do
    cue :master
    sleep 1
  end
ensure
  set :terminate, true
  cue :master
  sleep 3
  midi_all_notes_off
end
