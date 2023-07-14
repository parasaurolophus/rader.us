```ruby
# Copyright 2023 Kirk Rader. All rights reserved.

# Au Quai

use_random_seed 0
use_random_source :perlin
use_synth :sine

terminate = false

define :randomatic do |min_n, max_n, min_d, max_d|
  
  in_thread do
    
    loop do
      
      if terminate
        stop
      end
      
      n = note(rrand_i(min_n, max_n))
      d = rrand(min_d, max_d)
      
      # midi n, sustain: d
      play n
      sleep d
      
    end
  end
end

randomatic(45, 75, 0.25, 0.75)
sleep 100
terminate = true
```
