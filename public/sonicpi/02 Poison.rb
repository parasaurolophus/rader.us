# Copyright Kirk Rader 2023. All rights reserved.

# Poison

comment do
  live_loop :midi_piano do
    with_fx :reverb, room: 0.75 do
      use_real_time
      note, velocity = sync "/midi:mpk261_port_a:1/note_on"
      synth :dpulse, note: note, amp: velocity / 127.0
    end
  end
end

bpm = 16
tempo = 0.125
terminate = false

spr1 = (spread 3, bpm)
spr2 = (spread 5, bpm)
spr3 = (spread 7, bpm)
spr4 = (spread 1, bpm * 5)
#spr4 = (spread 2, bpm * 6)

enable_drums = false

in_thread do
  loop do
    sync :master
    midi_all_notes_off
    stop if terminate
    tick
    if enable_drums
      sample :tabla_ghe5, rate: (rrand 0.9, 1.1), amp: (rrand 0.9, 1.1), pan: -0.5 if spr1.look
      sample :tabla_ghe3, rate: (rrand 0.9, 1.1), amp: (rrand 0.9, 1.1) if spr2.look
      sample :tabla_ghe1, rate: (rrand 0.9, 1.1), amp: (rrand 0.9, 1.1), pan: 0.5 if spr3.look
    end
    with_random_seed 602801 do
      midi_note_on rrand_i(24, 84) if spr4.look
    end
  end
end

begin
  1000.times do
    cue :master
    sleep tempo
  end
ensure
  terminate = true
  cue :master
  midi_note_on 24, sustain: tempo
  sleep tempo
  midi_all_notes_off
end
