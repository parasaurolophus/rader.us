# Copyright 2023 Kirk Rader. All rights reserved.

# Echoing Pingala

comment do
  # tune midi synth
  loop do
    midi :A4, sustain: 0.99
    synth :fm, note: :A4, sustain: 0.99
    sleep 1
  end
ensure
  midi_all_notes_off
end

define :fibonacci do |n|
  if n < 2
    return 1
  else
    return (fibonacci (n - 1)) + (fibonacci (n - 2))
  end
end

uncomment do
  with_bpm 120 do
    notes1 = ((range 0, 8).map {|x| 23 + (fibonacci x)}).ring.repeat
    notes2 = ((range 0, 8).map {|x| 73 - (fibonacci x)}).ring.repeat
    beats1 = (spread 5, 8) + (spread 3, 8).rotate
    beats2 = (spread 4, 7).rotate + (spread 5, 7)
    midi :A4, sustain: 1
    sleep 16
    16.times do
      notes1.length.times do
        tick
        if beats1.look
          midi notes1[0], sustain: 2.0
          notes1 = notes1.rotate
        end
        if beats2.look
          midi notes2[0], sustain: 2.0
          notes2 = notes2.rotate
        end
        sleep 1
      end
      #notes1 = notes1.shuffle
      #notes2 = notes2.shuffle
    end
  ensure
    midi_all_notes_off
  end
end
