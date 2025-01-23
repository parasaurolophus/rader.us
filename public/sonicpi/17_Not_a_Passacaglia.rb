# Copyright 2024 Kirk Rader. All rights reserved.

# Not a Passacaglia

use_random_seed 20240312
use_random_source :white

four_pi = 4 * Math::PI
step = four_pi / 24.0
x = (range 0, four_pi, step)

bass = x.map{|x| 36 + (6.0 * (Math.sin(x/2.0) + Math.sin(x*2.0))).round }
treble = x.map{|x| 60 + (-6.0 * (Math.sin(x*2.0) + Math.sin(x/2.0))).round }

sus = (x.length / 5)
bass_beats = (spread sus * 3, x.length)
treble_beats = bass_beats

with_bpm 30 do
  midi (hz_to_midi 440), sustain: 0.1
  sleep 3
  3.times do
    x.length.times do
      tick
      if bass_beats.look
        midi bass[0], sustain: sus
        bass = bass.rotate
      end
      if treble_beats.look
        midi treble[0], sustain: sus
        treble = treble.rotate
      end
      sleep 1
    end
    #bass_beats = bass_beats.shuffle
    #treble_beats = treble_beats.shuffle
  end
ensure
  midi_all_notes_off
end
