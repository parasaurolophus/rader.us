# Copyright 2023 Kirk Rader. All rights reserved.

# Decomissioned

terminate = false

define :drone do |name|
  
  in_thread do
    
    with_random_source :perlin do
      
      with_sample_bpm name do
        
        loop do
          
          if terminate
            stop
          end
          
          rate = rrand(0.5, 2.0)
          delay = 1.0 / rate
          sample name, rate: rate
          sleep delay
          
        end
      end
    end
  end
end

define :melody do |name|
  
  in_thread do
    
    with_synth name do
      
      with_random_source :pink do
        
        loop do
          
          if terminate
            stop
          end
          
          duration = rrand(0.5, 2.0)
          play rrand(45, 65), sustain: duration
          sleep duration
          
        end
      end
    end
  end
end

name = :ambi_glass_hum

drone name
drone name
#drone name
melody :dark_ambience
melody :dark_ambience

sleep 100
terminate = true
