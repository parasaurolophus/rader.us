# Copyright 2023 Kirk Rader. All rights reserved.

# Unodecatonic

uncomment do
  with_random_seed 231110 do
    #notes = (range 0, 11).map {|x| x = (x / 2.0); 48 + (x * x).round}
    notes = (range 0, 11).map {|x| x = (x / 2.0); 60 - (x * x).round}
    notes = notes.reverse
    #beats = (spread ((notes.length / 3).round * 2), notes.length)
    beats = (spread ((notes.length / 4).round * 3), notes.length)
    beats = beats.reverse
    midi (hz_to_midi 440)
    sleep 10
    with_bpm 200 do
      (notes.length * 2).times do
        notes.length.times do
          tick
          if beats.look
            uncomment do
              midi notes[0], sustain: 0.9
            end
            notes = notes.rotate
          end
          sleep 1
        end
        notes = notes.rotate
        #notes = notes.rotate(-1)
      end
    ensure
      midi_all_notes_off
    end
  end
end