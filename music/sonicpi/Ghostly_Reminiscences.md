```ruby
# Copyright 2023 Kirk Rader. All rights reserved.

# Ghostly Reminiscences

live_loop :plingk do
    sample :perc_bell, rate: rrand(0.2, 2)
    sleep rrand(1, 5)
  end
  
  live_loop :clangk do
    sample :perc_bell2, rate: rrand(0.2, 2)
    sleep rrand(1, 5)
  end
  
  live_loop :eery do
    sample :ambi_haunted_hum, rate: rrand(0.2, 2)
    sleep rrand(0.75, 3)
  end
  
  live_loop :spooky do
    sample :ambi_glass_hum, rate: rrand(0.2, 2)
    sleep rrand(0.75, 3)
  end
  
  live_loop :chugga do
    sample :loop_industrial, rate: rrand(0.2, 2)
    sleep 5
  end
  
  live_loop :bzzz do
    sleep 2.5
    sample :loop_drone_g_97, rate: rrand(0.2, 2)
    sleep 7.5
  end
  ```
  