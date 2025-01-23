# Copyright Kirk Rader 2023. All rights reserved.

# Bridge Over the Great River

multiplier = 6
# multiplier = 12

define :ascending do |x|
  36 + (multiplier * Math.cbrt(x)).round
end

define :descending do |x|
  78 - (multiplier * Math.sqrt(x)).round
end

with_bpm 200 do
  with_synth :fm do
    with_random_seed 0 do
      with_random_source :pink do
        begin
          notes1 = (range 0.0, 32.0, 1.0).map {|x| (descending x)}.ring
          notes2 = (range 0.0, 32.0, 1.0).map {|x| (ascending x)}.ring
          beats1 = (spread (2 * (notes1.length / 3)), notes1.length) #.rotate
          beats2 = (spread (3 * (notes2.length / 4)), notes2.length) #.rotate(-1)
          play (hz_to_midi 440)
          midi (hz_to_midi 440)
          sleep notes1.length
          10.times do
            notes1.length.times do
              tick
              if beats1.look # beats2.look
                # play notes1[0], amp: 0.5, pan: -0.33
                midi notes1[0]
                notes1 = notes1.rotate
              end
              if beats1.look # beats2.look
                # play notes2[0], amp: 0.5, pan: 0.33
                midi notes2[0]
                notes2 = notes2.rotate
              end
              sleep 1
            end
            notes1 = notes1.shuffle
            notes2 = notes2.shuffle
          end
        ensure
          midi_all_notes_off
        end
      end
    end
  end
end
