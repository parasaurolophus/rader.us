# Copyright 2024 Kirk Rader. All rights reserved.

# Not a Sarabande

use_random_seed 20240309
use_random_source :white

four_pi = 4 * Math::PI
step = four_pi / 24.0
x = (range 0, four_pi, step)

bass = x.map{|x| 36 + (6.0 * (Math.tan(x) + Math.tan(x / 2))).round }.reverse
treble = x.map{|x| 60 + (6.0 * (Math.tan(x) + Math.tan(x / 2))).round }.reverse

#bass_beats = (spread (x.length / 4) * 3, x.length).shuffle
bass_beats = (spread (x.length / 3) * 2, x.length).shuffle
treble_beats = bass_beats.shuffle

with_bpm 90 do
  midi (hz_to_midi 440), sustain: 0.1
  sleep 9
  9.times do
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
    bass_beats = bass_beats.shuffle
    treble_beats = treble_beats.shuffle
  end
ensure
  midi_all_notes_off
end
