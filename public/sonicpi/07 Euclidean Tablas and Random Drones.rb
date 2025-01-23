# Copyright Kirk Rader 2023. All rights reserved.

# Euclidean Tablas and Random Drones

terminate = false

define :drone do |s, min, max|
  in_thread do
    with_random_source :perlin do
      with_sample_bpm s do
        loop do
          if terminate
            stop
          end
          r = rrand(min, max)
          sample s, rate: r, pan: rrand(-0.5, 0.5)
          sleep r
        end
      end
    end
  end
end

define :tablas do
  in_thread do
    spread_ghe = (spread 3, 4)
    spread_tas = (spread 5, 7)
    spread_tun = (spread 3, 16)
    spread_dhec = (spread 5, 16)
    ghe = (ring :tabla_ghe1, :tabla_ghe2, :tabla_ghe3, :tabla_ghe4, :tabla_ghe5, :tabla_ghe6, :tabla_ghe7, :tabla_ghe8)
    tas = (ring :tabla_tas1, :tabla_tas2, :tabla_tas3)
    tun = (ring :tabla_tun1, :tabla_tun2, :tabla_tun3)
    loop do
      if terminate
        stop
      end
      sample ghe.look, pan: -0.5 if spread_ghe.tick
      sample tas.look, pan: -0.25 if spread_tas.look
      sample tun.look, pan: 0.25 if spread_tun.look
      sample :tabla_dhec, pan: 0.5 if spread_dhec.look
      sleep 0.25
    end
  end
end

tablas
sleep 10
drone :ambi_dark_woosh, 1, 2
sleep 10
drone :ambi_haunted_hum, 1, 1.5
sleep 10
drone :ambi_glass_hum, 0.75, 1.25
sleep 90
terminate = true
