```ruby
# Copyright 2023 Kirk Rader. All rights reserved.

# Farandole Lamentoso

midi_all_notes_off
sleep 1

arp_synth = :dpulse
bass_synth = :bass_foundation
seed = 0
next_seed = 7
arps = 2
arp_tempo = 0.33
send_midi_arp = false
send_midi_bass = false
play_notes = true

define :play_arp do |r, c|
  
  use_synth arp_synth
  
  c.times do
    
    use_random_seed seed
    seed = seed + next_seed
    
    r.shuffle.length.times do
      
      n = r.tick
      
      if send_midi_arp
        midi n
      end
      
      if play_notes
        play n
      end
      
      sleep arp_tempo
      
    end
  end
end

define :play_bass do |n, s|
  
  use_synth bass_synth
  
  if send_midi_bass
    midi n, sustain: s
  end
  
  if play_notes
    play n, sustain: s
  end
  
end

define :play_bass_melody do |n, s|
  
  in_thread do
    
    n.length.times do
      
      play_bass n.tick, s
      sleep s
      
    end
  end
end

a_min = (ring :A3, :C4, :E4)
d_min = (ring :A3, :D4, :F4)
g_maj = (ring :G3, :B3, :D4)
c_maj = (ring :G3, :C4, :E4)
f_maj = (ring :F3, :A3, :C4)
b_min = (ring :F3, :B3, :D4)
a_min_inv = (ring :E3, :A3, :C4)
e_maj_sus = (ring :E3, :A3, :B3)
e_maj = (ring :E3, :Gs3, :B3)

bass_melody = (ring :C3, :A2, :B2, :Gs2)

2.times do
  
  play_bass :A3, (arps * a_min.length * arp_tempo)
  play_arp a_min, arps
  
  play_bass :D3, (arps * d_min.length * arp_tempo)
  play_arp d_min, arps
  
  play_bass :G3, (arps * g_maj.length * arp_tempo)
  play_arp g_maj, arps
  
  play_bass :C3, (arps * c_maj.length * arp_tempo)
  play_arp c_maj, arps
  
  play_bass :F3, (arps * f_maj.length * arp_tempo)
  play_arp f_maj, arps
  
  play_bass :B3, (arps * b_min.length * arp_tempo)
  play_arp b_min, arps
  
  play_bass_melody bass_melody, (a_min_inv.length * arp_tempo)
  play_arp a_min_inv, arps
  
  play_arp e_maj_sus, 1
  play_arp e_maj, 1
  
end

play_bass :A2, (arps * a_min_inv.length * arp_tempo)
play_arp a_min_inv, arps

if send_midi_arp
  midi :A4
end

if play_notes
  play :A4
end

play_bass :A2, (arps * a_min_inv.length * arp_tempo)
```