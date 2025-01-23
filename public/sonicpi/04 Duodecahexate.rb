# Copyright 2023 Kirk Rader. All rights reserved.

# Duodecahexate

uncomment do
  #notes = (range 42, 54).reflect.butlast
  #notes = (range 54, 42).reflect.butlast
  #notes = (range 36, 60, 2).reflect.butlast
  notes = (range 84, 24, 5).butlast.repeat
  #beats = (spread ((notes.length / 3).round * 2), notes.length)
  #beats = (spread ((notes.length / 4).round * 3), notes.length)
  beats = (spread ((notes.length / 6).round * 5), notes.length)
  #beats = beats.rotate((notes.length / 3).round)
  midi (hz_to_midi 440)
  sleep 10
  with_bpm 200 do
    notes.length.times do
      notes.length.times do
        tick
        if beats.look
          midi notes[0], sustain: 0.9
          notes = notes.rotate
        end
        sleep 1
      end
      #notes = notes.rotate
      notes = notes.rotate(-1)
    end
  ensure
    midi_all_notes_off
  end
end
