# Copyright Kirk Rader 2023. All rights reserved.

# The Three Branches

define :ascending do |x|
  36 + (12 * Math.cbrt(x)).round
end

define :descending do |x|
  78 - (12 * Math.sqrt(x)).round
end

begin
  
  midi (hz_to_midi 440)
  sleep 5
  
  with_bpm 240 do
    with_random_seed 0 do
      with_random_source :pink do
        notes1 = (range 0.0, 32.0, 1.0).map {|x| (descending x)}.ring
        notes2 = (range 0.0, 32.0, 1.0).map {|x| (ascending x)}.ring
        beats1 = (spread (3 * (notes1.length / 4)), notes1.length)
        beats2 = (spread (2 * (notes2.length / 3)), notes2.length)
        24.times do
          notes1.length.times do
            tick
            if beats1.look
              midi notes1[0]
              notes1 = notes1.rotate
            end
            if beats2.look
              midi notes2[0]
              notes2 = notes2.rotate
            end
            sleep 1
          end
        end
      end
    end
  end
  
ensure
  midi_all_notes_off
end
