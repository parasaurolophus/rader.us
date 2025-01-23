# Copyright 2023 Kirk Rader. All rights reserved.

# Surjection

uncomment do
  #with_random_seed 0 do
  with_random_seed 231105 do
    #notes = (range 24, 84, 5).mirror
    #notes = (range 60, 36, 2).mirror
    notes = (range 42, 55, 1).reflect.butlast
    #beats = (spread ((notes.length / 6).round * 5), notes.length)
    #beats = (spread ((notes.length / 4).round * 3), notes.length)
    beats = (spread ((notes.length / 3).round * 2), notes.length)
    trans_range = (notes.length / 2).round
    trans = (range -trans_range, trans_range, 1).shuffle
    puts trans.length
    midi (hz_to_midi 440)
    sleep 10
    with_bpm 200 do
      phrase = 0
      notes.length.times do
        notes.length.times do
          tick
          if beats.look
            comment do
              midi notes[0], sustain: 0.9
            end
            uncomment do
              with_transpose trans[phrase] do
                midi notes[0], sustain: 0.9
              end
            end
            notes = notes.rotate
          end
          sleep 1
        end
        phrase = inc phrase
      end
    ensure
      midi_all_notes_off
    end
  end
end