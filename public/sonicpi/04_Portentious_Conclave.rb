# Copyright 2024 Kirk Rader

# Portentious Conclave

terminate = false

notes1 = (scale :a2, :melodic_minor_asc).butlast + (scale :a2, :melodic_minor_desc).reverse.butlast
notes2 = (scale :a4, :melodic_minor_desc).reverse.butlast + (scale :a4, :melodic_minor_asc).butlast
notes3 = (scale :a1, :melodic_minor_asc, num_octaves: 2).butlast.rotate(1)
notes4 = (scale :a5, :melodic_minor_desc, num_octaves: 2).reverse.butlast.rotate(2)

last1 = :a2
last2 = :a4
last3 = notes3[0]
last4 = notes4[0]

beats1 = (spread 3, 7) + (spread 5, 9)
beats2 = (spread 5, 9) + (spread 3, 7)
beats3 = (spread 3, 9) + (spread 5, 7)
beats4 = (spread 5, 7) + (spread 3, 9)

in_thread do
  midi_mode :poly
  with_bpm 120 do
    midi (hz_to_midi 440), sustain: 0.5
    sleep 10
    10.times do
      notes1.length.times do
        cue :master
        sleep 1
      end
    end
    cue :master
    sleep 1
    terminate = true
    cue :master
    sleep 1
  ensure
    midi_all_notes_off
  end
end

comment do
  in_thread do
    loop do
      sync :master
      break if terminate
      tick
      midi notes1.look, sustain: 0.5 if beats1.look
    end
    midi last1, sustain: 0.5
  end
end

comment do
  in_thread do
    loop do
      sync :master
      break if terminate
      tick
      midi notes2.look, sustain: 0.5 if beats2.look
    end
    midi last2, sustain: 0.5
  end
end

uncomment do
  in_thread do
    loop do
      sync :master
      break if terminate
      tick
      midi notes3.look, sustain: 0.5 if beats3.look
    end
    midi last3, sustain: 0.5
  end
end

uncomment do
  in_thread do
    loop do
      sync :master
      break if terminate
      tick
      midi notes4.look, sustain: 0.5 if beats4.look
    end
    midi last4, sustain: 0.5
  end
end
