# Copyright 2024 Kirk Rader. All rights reserved.

# Also Not a Minuet

use_random_seed 20240315
use_random_source :white

four_pi = 4 * Math::PI
step = four_pi / 24.0
x = (range 0, four_pi, step).take(24)

uncomment do
  bass = x.map{|x| 36 + (6.0 * (Math.cos(x*2.0) + Math.cos(x/2.0))).round }
  treble = x.map{|x| 60 + (-6.0 * (Math.sin(x*2.0) + Math.sin(x/2.0))).round }
  
  bass_beats = (spread (x.length / 3) * 2, x.length)
  treble_beats = (spread (x.length / 4) * 3, x.length).rotate
  
  with_bpm 180 do
    midi (hz_to_midi 440), sustain: 0.1
    sleep 18
    18.times do
      x.length.times do
        tick
        puts x.length
        if bass_beats.look
          midi bass[0], sustain: 0.9
          bass = bass.rotate
        end
        if treble_beats.look
          midi treble[0], sustain: 0.9
          treble = treble.rotate
        end
        sleep 1
      end
      bass_beats = bass_beats.rotate
      treble_beats = treble_beats.rotate
    end
  ensure
    midi_all_notes_off
  end
end

comment do
  bass = x.map{|x| 36 + (6.0 * (Math.sin(x*2.0) + Math.sin(x/2.0))).round }
  treble = x.map{|x| 60 + (-6.0 * (Math.cos(x*2.0) + Math.cos(x/2.0))).round }
  
  bass_beats = (spread (x.length / 3) * 2, x.length)
  treble_beats = (spread (x.length / 4) * 3, x.length).rotate
  
  with_bpm 120 do
    midi (hz_to_midi 440), sustain: 0.1
    sleep 12
    12.times do
      x.length.times do
        tick
        puts x.length
        if bass_beats.look
          midi bass[0], sustain: 0.9
          bass = bass.rotate
        end
        if treble_beats.look
          midi treble[0], sustain: 0.9
          treble = treble.rotate
        end
        sleep 1
      end
      bass_beats = bass_beats.rotate
      treble_beats = treble_beats.rotate
    end
  ensure
    midi_all_notes_off
  end
end

comment do
  bass = x.map{|x| 36 + (6.0 * (Math.cos(x*2.0) + Math.sin(x/2.0))).round }
  treble = x.map{|x| 60 + (-6.0 * (Math.sin(x*2.0) + Math.cos(x/2.0))).round }
  
  bass_beats = (spread (x.length / 3) * 2, x.length)
  treble_beats = (spread (x.length / 4) * 3, x.length).rotate
  
  with_bpm 30 do
    midi (hz_to_midi 440), sustain: 0.1
    sleep 3
    3.times do
      x.length.times do
        tick
        puts x.length
        if bass_beats.look
          midi bass[0], sustain: 0.9
          bass = bass.rotate
        end
        if treble_beats.look
          midi treble[0], sustain: 0.9
          treble = treble.rotate
        end
        sleep 1
      end
      bass_beats = bass_beats.rotate
      treble_beats = treble_beats.rotate
    end
  ensure
    midi_all_notes_off
  end
end
