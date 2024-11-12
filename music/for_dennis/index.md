_Copyright &copy; 2024 Kirk Rader_

# Anatomy of a Digital Musical Composition

[For Dennis](./For_Dennis.mp3">20_For_Dennis.mp3) <audio controls>
    <source src="./For_Dennis.mp3" type="audio/mpeg"/>
</audio>

_For Dennis_ is the result of a recent conversation with a friend. He described
an idea he had for a musical composition memorializing the thousands of victims
of the terrorist attacks on September 11, 2001. His description of the piece he
had in mind was the inspiration for the following bit of Ruby code:

```ruby
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
```

When executed by [Sonic Pi](https://sonic-pi.net/), the output of the preceding
27 lines of code is the 44ish minutes of _For Dennis_.

My goal for this work was to produce music that is meditative rather than
entertaining or emotionally rousing. It is entirely algorithmic in conception
and execution. The only "artistic" choices made were the exact parameters for
things like the pitch of each voice, the number of beats each voice is played
over the course of the work, and so on.

In the spirit of my friend's original conception, I chose numbers which
memorialize the victims of 9/11. The primary "heartbeat" is played exactly 2606
times, once for each person who died that day in the World Trade Center towers.
The other voices are played a number of times each that corresponds to the
victims in each of the hijacked planes and on the ground at the other affected
locations. If you listen to the whole piece, by the end you will have heard one
note played for each person murdered that day. In addition, the tempo speeds up
and down subliminally at first, then quite noticeably, over the course of the
piece in four waves, once for each hijacked plane and the people killed by each
crash.

None of this is exactly what my friend described in his vision for such a
piece. But I was definitely inspired by his ideas to create a work of my own
that I hope is consonant with his intention.