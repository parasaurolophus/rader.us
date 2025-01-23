# Copyright 2023 Kirk Rader. All rights reserved.

# Greed's Meed

comment do
  begin
    loop do
      synth :sine, note: (hz_to_midi 440), amp: 0.5
      midi (hz_to_midi 440)
      sleep 1
    end
  ensure
    midi_all_notes_off
  end
end

uncomment do
  in_thread do
    with_fx :reverb, room: 0.0, room_slide: 1.0 do |reverb|
      with_fx :ring_mod, mix: 0.0, mix_slide: 1.0 do |ring_mod|
        with_synth :beep do
          notes1 = (range -Math::PI, Math::PI, ((2.0 * Math::PI) / 8.0)).map {|x| (12.0 * Math.sin(x)).round + 60}.ring
          notes2 = (range -Math::PI, Math::PI, ((2.0 * Math::PI) / 8.0)).map {|x| (12.0 * Math.cos(x)).round + 36}.ring
          notes3 = (range -1.0, 1.0, (2.0 / 8.0)).map {|x| (12.0 * Math.tan(x)).round + 48}.ring
          notes4 = (range -1.0, 1.0, (2.0 / 16.0)).map {|x| (12.0 * Math.tan(x)).round + 48}.ring
          notes1 = notes1 + notes1.drop(1).butlast
          notes2 = notes2 + notes2.drop(1).butlast
          notes3 = notes3 + notes3.drop(1).butlast
          notes4 = notes4.take(notes1.length)
          beats1 = (spread (notes1.length - 3), notes1.length)
          beats2 = beats1.reverse
          beats3 = (spread (notes1.length - 2), (notes1.length - 1))
          beats4 = beats3.reverse
          max_tempo = notes1.length * notes1.length
          accelerando = (max_tempo / notes1.length)
          tempo = (max_tempo / 2.0)
          ring_mod_mix = 0.0
          room_size = 0.0
          control_step = (1.0 / notes1.length)
          notes1.length.times do
            use_bpm tempo
            notes1.length.times do
              tick
              if beats1.look
                uncomment do
                  #midi notes1[0]
                  play notes1[0], amp: 0.5, pan: -0.5
                  notes1 = notes1.rotate
                end
              end
              if beats2.look
                uncomment do
                  #midi notes2[0]
                  play notes2[0], amp: 0.5, pan: 0.25
                  notes2 = notes2.rotate
                end
              end
              if beats3.look
                uncomment do
                  #midi notes3[0]
                  play notes3[0], amp: 0.5, pan: 0.25
                  notes3 = notes3.rotate
                end
              end
              if beats4.look
                uncomment do
                  #midi notes4[0]
                  play notes4[0], amp: 0.5, pan: 0.5
                  notes4 = notes4.rotate
                end
              end
              sleep 1
            end
            tempo = tempo + accelerando
            ring_mod_mix = ring_mod_mix + control_step
            room_size = room_size + control_step
            control ring_mod, mix: ring_mod_mix
            control reverb, room: room_size
          end
          sleep 3 * notes1.length
        end
      end
    ensure
      midi_all_notes_off
    end
  end
end