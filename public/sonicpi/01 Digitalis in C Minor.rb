# Copyright Kirk Rader 2023. All rights reserved.

# Digitalis in C Minor

beats_per_measure = 16
tempo = 0.2

enable_floor = true
enable_arp_1 = true
enable_arp_2 = true
enable_bass = true
enable_midi = false

################################################################################

chords1 = (ring
           (chord :C4, :minor),
           (chord :G3, :major, invert: 1),
           (chord :Bb3, :minor),
           (chord :F3, :major, invert: 1),
           (chord :A3, :minor),
           (chord :C3, :minor, invert: 2),
           (chord :G3, :augmented),
           (chord :G3, :major),
           (chord :C3, :minor, invert: 1))

chords_spread_1 = (spread 1, beats_per_measure)
5.times do
  chords_spread_1 = chords_spread_1 + (spread 1, beats_per_measure)
end
chords_spread_1 = chords_spread_1 + (spread 1, beats_per_measure - 1)
chords_spread_1 = chords_spread_1 + (spread 1, 1)
chords_spread_1 = chords_spread_1 + (spread 1, beats_per_measure)

bass1 = (ring :C4, :G3, :Bb3, :F3, :A3, :C3, :G3, :G3, :C3)

arpeggio1 = (ring
             beats_per_measure,
             beats_per_measure,
             beats_per_measure,
             beats_per_measure,
             beats_per_measure,
             beats_per_measure,
             beats_per_measure - 1,
             1,
             beats_per_measure)

chords_sustain_1 = arpeggio1.map { |measure| measure * tempo }

################################################################################

chords2 = (ring
           (chord :F3, :minor),
           (ring :D3, :Ab3, :C4),
           (chord :G2, :sus4, invert: 1),
           (chord :G2, :major, invert: 1),
           (chord :C3, :major, invert: 1))

chords_spread_2 = (spread 1, beats_per_measure / 2)
chords_spread_2 = chords_spread_2 + (spread 1, beats_per_measure / 2)
3.times do
  chords_spread_2 = chords_spread_2 + (spread 1, beats_per_measure)
end

arpeggio2 = (ring
             beats_per_measure / 2,
             beats_per_measure /2,
             beats_per_measure,
             beats_per_measure,
             beats_per_measure)

chords_sustain_2 = arpeggio2.map { |measure| measure * tempo }

bass2 = (ring :F3, :D3, :G2, :G2, :C3)

################################################################################

phrase_length_1 = (chords1.length - 1) * beats_per_measure
phrase_length_2 = (chords2.length - 1) * beats_per_measure
total_beats_1 = phrase_length_1 * 4
total_beats_2 = phrase_length_2
last_beat = (total_beats_1 + total_beats_2) - 1
beat_count = 0
phrase_count = 0
terminate = false

################################################################################

define :play_arp do |notes, arp_count, arp_spread, p|
  in_thread do
    notes = notes + notes.drop(1).reverse.drop(1)
    arp_count.times do
      if arp_spread.tick
        if enable_midi
          midi notes[0], sustain: tempo * 0.75
        else
          synth :kalimba, note: notes[0], sustain: tempo, amp: 5, pan: p
        end
        notes = notes.rotate
      end
      sleep tempo
    end
  end
end

################################################################################

in_thread do
  with_fx :reverb, room: 1.0 do
    loop do
      sync :master
      stop if terminate
      if phrase_count < 4
        if chords_spread_1.tick
          if enable_floor
            with_synth :hollow do
              play chords1[0], sustain: chords_sustain_1[0]
            end
          end
          if enable_bass && phrase_count > 2
            with_octave -1 do
              if enable_midi
                midi bass1[0], sustain: chords_sustain_1[0] * 0.75
              else
                synth :bass_foundation, note: bass1[0], sustain: chords_sustain_1[0], amp: 0.2 * (phrase_count - 1)
                synth :bass_highend, note: bass1[0], sustain: chords_sustain_1[0] / 2, amp: 0.1 * (phrase_count - 1)
              end
            end
          end
          with_random_source :white do
            if enable_arp_1 && phrase_count > 0
              play_arp chords1[0], arpeggio1[0], (spread (rrand_i 5, beats_per_measure), beats_per_measure), -0.5
            end
            if enable_arp_2 && phrase_count > 1
              play_arp chords1[0], arpeggio1[0], (spread (rrand_i 5, beats_per_measure), beats_per_measure), 0.5
            end
          end
          chords1 = chords1.rotate
          bass1 = bass1.rotate
          chords_sustain_1 = chords_sustain_1.rotate
          arpeggio1 = arpeggio1.rotate
        end
      else
        if chords_spread_2.tick
          s = chords_sustain_2[0]
          s = s * 2 if beat_count = last_beat
          if enable_floor
            with_synth :hollow do
              play chords2[0], sustain: s
            end
          end
          if enable_bass
            with_octave -1 do
              if enable_midi
                midi bass2[0], sustain: s
              else
                synth :bass_foundation, note: bass2[0], sustain: s, amp: 0.2 * (phrase_count - 1)
                synth :bass_highend, note: bass2[0], sustain: s / 2, amp: 0.1 * (phrase_count - 1)
              end
            end
          end
          with_random_source :white do
            if enable_arp_1
              play_arp chords2[0], arpeggio2[0], (spread (rrand_i 5, beats_per_measure), beats_per_measure), -0.5
            end
            if enable_arp_2
              play_arp chords2[0], arpeggio2[0], (spread (rrand_i 5, beats_per_measure), beats_per_measure), 0.5
            end
          end
          chords2 = chords2.rotate
          bass2 = bass2.rotate
          chords_sustain_2 = chords_sustain_2.rotate
          arpeggio2 = arpeggio2.rotate
        end
      end
    end
  end
end

################################################################################

synth :beep, note: :C4, sustain: 1
midi :C4, sustain: 1

sleep beats_per_measure * tempo

begin
  total_beats_1.times do
    puts beat_count
    puts phrase_count
    cue :master
    beat_count = inc beat_count
    phrase_count = inc phrase_count if ((beat_count % phrase_length_1) == 0)
    sleep tempo
  end
  total_beats_2.times do
    puts beat_count
    puts phrase_count
    cue :master
    beat_count = inc beat_count
    phrase_count = inc phrase_count if ((beat_count % phrase_length_2) == 0)
    sleep tempo
  end
ensure
  midi_all_notes_off
  terminate = true
  cue :master
end
