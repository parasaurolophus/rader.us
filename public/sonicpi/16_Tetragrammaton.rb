# Copyright 2024 Kirk Rader. All rights reserved.

# Tetragrammaton

use_random_seed 20240309
use_random_source :white

four_pi = 4 * Math::PI
step = four_pi / 24.0
x = (range 0, four_pi, step)

bass = x.map{|x| 36 + (6.0 * (Math.sin(x) + Math.sin(x))).round }
treble = x.map{|x| 60 + (-6.0 * (Math.sin(x) + Math.sin(x))).round }

bass_beats = (spread (x.length / 5) * 4, x.length - 1)
treble_beats = (spread (x.length / 5) * 4, x.length + 1)

with_bpm 180 do
  midi (hz_to_midi 440), sustain: 0.1
  sleep 18
  18.times do
    x.length.times do
      tick
      if bass_beats.look
        midi bass[0], sustain: 1.1
        bass = bass.rotate
      end
      if treble_beats.look
        midi treble[0], sustain: 1.1
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
