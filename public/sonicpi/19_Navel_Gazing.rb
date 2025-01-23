# Copyright 2024 Kirk Rader. All rights reserved.

# Navel Gazing

use_random_source :white

comment do
  
  with_random_seed 19641026 do
    
    notes = (chord :c1, :minor7, num_octaves: 3).shuffle
    beats = (spread 5, 5) + (spread 1, 5)
    
    with_bpm 30 do
      midi (hz_to_midi 440), sustain: 0.1
      sleep 3
      3.times do
        notes.length.times do
          tick
          if beats.look
            midi notes[0], sustain: 0.9
            notes = notes.rotate
          end
          sleep 1
        end
        beats = beats.rotate
      end
    ensure
      midi_all_notes_off
    end
  end
end

comment do
  
  with_random_seed 19600128 do
    
    notes = (chord :c2, :minor7, num_octaves: 3).shuffle
    beats = (spread 2, 3) + (spread 1, 3)
    
    with_bpm 60 do
      midi (hz_to_midi 440), sustain: 0.1
      sleep 6
      6.times do
        notes.length.times do
          tick
          if beats.look
            midi notes[0], sustain: 0.9
            notes = notes.rotate
          end
          sleep 1
        end
        beats = beats.rotate
      end
    ensure
      midi_all_notes_off
    end
  end
end

uncomment do
  
  with_random_seed 20240317 do
    
    notes = (chord :c3, :minor7, num_octaves: 3).shuffle
    beats = (spread 3, 4) + (spread 1, 4)
    
    with_bpm 180 do
      midi (hz_to_midi 440), sustain: 0.1
      sleep 18
      18.times do
        notes.length.times do
          tick
          if beats.look
            midi notes[0], sustain: 0.9
            notes = notes.rotate
          end
          sleep 1
        end
        beats = beats.rotate
      end
    ensure
      midi_all_notes_off
    end
  end
end
