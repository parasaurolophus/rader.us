# Copyright 2024 Kirk Rader

# For Dennis

base_tempo = 60.0
total_ticks = 2606.0
adjustment = (2.0 * Math::PI) / total_ticks
volume = 0.5

define :adjust do |x|
  return 1 + (Math.cos( 4 * x * adjustment) / 2)
end

in_thread do
  beats1 = (spread 87, total_ticks)
  beats2 = (spread 60, total_ticks)
  beats3 = (spread 59, total_ticks)
  beats4 = (spread 125, total_ticks)
  beats5 = (spread 40, total_ticks)
  with_bpm base_tempo do
    total_ticks.times do
      tick
      synth :pretty_bell, note: 60, pan:  0.00, amp: volume
      synth :pretty_bell, note: 48, pan: -0.66, amp: volume, sustain: 20 if beats1[look]
      synth :pretty_bell, note: 54, pan: -0.33, amp: volume, sustain: 30 if beats2[look]
      synth :pretty_bell, note: 66, pan:  0.33, amp: volume, sustain: 29 if beats3[look]
      synth :pretty_bell, note: 42, pan:  0.66, amp: volume, sustain: 15 if beats4[look]
      synth :pretty_bell, note: 72, pan:  0.00, amp: volume, sustain: 25 if beats5[look]
      sleep adjust(look)
    end
  end
end
