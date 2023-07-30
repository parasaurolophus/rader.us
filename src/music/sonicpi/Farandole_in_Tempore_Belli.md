```ruby
# Copyright Kirk Rader 2023. All rights reserved.

# Farandole in Tempore Belli

# sleep duration for each beat
tempo = 0.2

# beats per measure
measure = 16

# drum rhythm
spread_lo = (spread 5, measure)
spread_mid = (spread 7, measure)
spread_hi = (spread 3, measure / 2 - 1)
spread_bass = (spread 1, measure)

# start of each measure
spread_measures = (spread 1, measure)

# arpeggio rhythms
spread_arp_1 = (spread measure / 3, measure / 2)
spread_arp_2 = (spread (measure / 2) + 1, measure)

chords = (ring
          (chord :a4, :minor),
          (chord :d4, :minor, invert: 2),
          (chord :g4, :major),
          (chord :c4, :major, invert: 2),
          (chord :f4, :major),
          (chord :b3, :diminished, invert: 2),
          (chord :a3, :minor, invert: 2),
          (chord :e4, :sus4),
          (chord :e4, :major))

bass = (ring
        (note :a2),
        (note :d1),
        (note :g1),
        (note :c1),
        (note :f2),
        (note :b1),
        (note :c2),
        (note :e1),
        (note :e1))

# duration of one measure
measure_hold = tempo * measure

# number of beats in a full phrase
phrase_length = chords.length * measure

# number of beats to repeat the full phrase 6 times
repeats = phrase_length * 6

# current beat number being played each time :master is cued
count = 0

# current phrase number being played each time :master is cued
phrase = 0

# stop threads next time :master is cued
terminate = false

# flags to allow different voices to be played out
# as separate tracks
enable_drums = true
enable_ambience = true
enable_chords = true
enable_arp_1 = true
enable_arp_2 = true
enable_bass = true

# flag to emit single-note voices (arp_1,
# arp_2 and bass) as midi events rather
# than playing audio output
enable_midi = false

# play arpeggio for 1 measure
define :play_arp do |notes, spread|
  in_thread do
    with_fx :reverb, room: 0.75 do
      with_synth :chiplead do
        measure.times do
          stop if terminate
          if spread.tick
            if enable_midi
              midi notes[0], sustain: tempo
            else
              play notes[0], sustain: tempo, amp: phrase * 0.1
            end
            notes = notes.rotate
          end
          sleep tempo
        end
      end
    end
  end
end

with_fx :reverb, room: 0.75 do
  
  begin
    
    # chords & arpeggios
    in_thread do
      with_random_source :perlin do
        loop do
          
          # await next beat
          sync :master
          stop if terminate
          
          # only take action once per measure
          if spread_measures.tick
            
            # play current chord using ambient voice
            with_synth :dark_ambience do
              with_transpose -12 do
                play chords[0], sustain: measure_hold, amp: 1.5 * phrase, pan: rrand(-0.5, 0.5) if enable_ambience && phrase > 0
              end
            end
            
            # play current chord as arpeggio using first pattern
            play_arp chords[0], spread_arp_1 if enable_arp_1 && phrase > 1
            
            # play current chord as arpeggio using second pattern
            play_arp chords[0], spread_arp_2 if enable_arp_2 && phrase > 2
            
            # play current cord using melodic voice
            with_synth :chipbass do
              play chords[0], sustain: measure_hold * 0.8, amp: phrase * 0.1, pan: rrand(-0.5, 0.5) if enable_chords && phrase > 2
            end
            
            # play current note in bass line
            with_synth :bass_foundation do
              if enable_bass && phrase > 3
                if enable_midi
                  midi bass[0], sustain: measure_hold
                else
                  play bass[0], sustain: measure_hold, pan: rrand(-0.5, 0.5)
                end
              end
            end
            
            # advance to the next measure
            chords = chords.rotate
            bass = bass.rotate
          end
        end
      end
    end
    
    # percussion
    if enable_drums
      in_thread do
        with_random_source :white do
          loop do
            sync :master
            stop if terminate
            tick
            sample :drum_tom_lo_hard, rate: rrand(0.75, 1.25), amp: rrand(0.2, 0.5), pan: -0.25 if spread_lo.look
            sample :drum_tom_mid_hard, rate: rrand(0.75, 1.25), amp: rrand(0.2, 0.5), pan: 0.25 if spread_mid.look
            sample :drum_tom_hi_hard, rate: rrand(0.75, 1.25), amp: rrand(0.2, 0.5) if spread_hi.look
            sample :drum_bass_hard, rate: rrand(0.75, 1.25), amp: rrand(0.5, 0.8) if spread_bass.look
          end
        end
      end
    end
    
    # clapper for syncing tracks in a DAW
    with_synth :beep do
      play :a4
      midi :a4, sustain: 1
    end
    
    # give space after clapper
    sleep measure_hold
    
    # master clock for all threads
    repeats.times do
      cue :master
      count = count + 1
      phrase = phrase + 1 if count % phrase_length == 0
      sleep tempo
    end
    
  ensure
    
    terminate = true
    sleep tempo * 2
    cue :master
    sleep tempo * 2
    midi_all_notes_off if enable_midi
    
  end
end
```
