# Copyright 2023 Kirk Rader. All rights reserved.

# Reversal

mode = :major
synth = :fm
phrases = 24
tempo = 180

uncomment do
  in_thread do
    notes = (scale :A2, mode).reverse
    beats = (spread 3, 4)
    with_synth synth do
      play notes[0], amp: 0.5, pan: 0.25, sustain: 0.9
      #midi notes[0], sustain: 0.9
      sleep 10
      with_bpm tempo do
        phrases.times do
          notes.length.times do
            if beats.tick
              play notes[0], amp: 0.5, pan: 0.25, sustain: 0.9
              #midi notes[0], sustain: 0.9
              notes = notes.rotate
            end
            sleep 1
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end

uncomment do
  in_thread do
    notes = (scale :A3, mode).rotate(-2).reverse
    beats = (spread 4, 5)
    with_synth synth do
      play notes[0], amp: 0.5, pan: 0.0, sustain: 0.9
      #midi notes[0], sustain: 0.9
      sleep 10
      with_bpm tempo do
        phrases.times do
          notes.length.times do
            if beats.tick
              play notes[0], amp: 0.5, pan: 0.0, sustain: 0.9
              #midi notes[0], sustain: 0.9
              notes = notes.rotate
            end
            sleep 1
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end

uncomment do
  in_thread do
    notes = (scale :A4, mode).rotate(-4).reverse
    beats = (spread 5, 7)
    with_synth synth do
      play notes[0], amp: 0.5, pan: -0.25, sustain: 0.9
      #midi notes[0], sustain: 0.9
      sleep 10
      with_bpm tempo do
        phrases.times do
          notes.length.times do
            if beats.tick
              play notes[0], amp: 0.5, pan: -0.25, sustain: 0.9
              #midi notes[0], sustain: 0.9
              notes = notes.rotate
            end
            sleep 1
          end
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end

uncomment do
  in_thread do
    notes = (scale :A2, mode, num_octaves: 2)
    beats = (spread 6, 9)
    with_synth synth do
      play notes[0], amp: 0.5, pan: -0.25, sustain: 0.9
      #midi notes[0], sustain: 0.9
      sleep 10
      with_bpm tempo do
        n = ((phrases / 2) + 1)
        n.times do
          notes.length.times do
            if beats.tick
              play notes[0], amp: 0.5, pan: -0.25, sustain: 0.9
              #midi notes[0], sustain: 0.9
              notes = notes.rotate
            end
            sleep 1
          end
          beats = beats.rotate
        end
      end
    end
  ensure
    midi_all_notes_off
  end
end
