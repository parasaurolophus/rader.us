# Copyright 2023 Kirk Rader. All rights reserved.

# Duodecatonic

uncomment do
  notes = (range 84, 24)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 120 do
    with_random_seed 0 do
      with_random_source :dark_pink do
        2.times do
          notes = notes.shuffle
          notes.length.times do
            midi notes[0], sustain: 0.9
            notes = notes.rotate
            sleep 1
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end

comment do
  notes = (range 24, 84)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 120 do
    with_random_seed 1 do
      with_random_source :pink do
        2.times do
          notes = notes.shuffle
          notes.length.times do
            midi notes[0], sustain: 0.9
            notes = notes.rotate
            sleep 1
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end

comment do
  notes = (range 84, 24, 2)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 120 do
    with_random_seed 2 do
      with_random_source :light_pink do
        4.times do
          notes = notes.shuffle
          notes.length.times do
            midi notes[0], sustain: 0.9
            notes = notes.rotate
            sleep 1
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end

comment do
  notes = (range 24, 84, 2)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 120 do
    with_random_seed 3 do
      with_random_source :white do
        4.times do
          notes = notes.shuffle
          notes.length.times do
            midi notes[0], sustain: 0.9
            notes = notes.rotate
            sleep 1
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end
