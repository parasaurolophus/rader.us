# Copyright 2024 Kirk Rader

# Example 03

use_random_seed 10
use_random_source :white

terminate = false

# master clock
in_thread do
  with_bpm 120 do
    midi (hz_to_midi 440)
    sleep 10
    180.times do
      cue :master
      sleep 1
    end
  ensure
    terminate = true
    cue :master
    sleep 1
    midi_all_notes_off
  end
end

# track 1 (low toms hard)
comment do
  in_thread do
    with_bpm 120 do
      beats = (spread 2, 5).rotate(2)
      loop do
        sync :master
        stop if terminate
        tick
        midi 36 if beats.look
      end
    ensure
      midi 36
      sleep 1
    end
  end
end

# track 2 (low toms soft)
comment do
  in_thread do
    with_bpm 120 do
      beats = (spread 2, 5).rotate(2)
      loop do
        sync :master
        stop if terminate
        tick
        midi 36 if !beats.look
      end
    end
  end
end

# track 3 (high toms hard)
comment do
  in_thread do
    with_bpm 120 do
      beats = (spread 3, 7).rotate(1)
      loop do
        sync :master
        stop if terminate
        tick
        midi 48 if beats.look
      end
    end
  end
end

# track 4 (high toms soft)
comment do
  in_thread do
    with_bpm 120 do
      beats = (spread 3, 7).rotate(1)
      loop do
        sync :master
        stop if terminate
        tick
        midi 48 if !beats.look
      end
    end
  end
end

# track 5 (bass)
comment do
  in_thread do
    with_bpm 120 do
      notes = (range 20, 31).shuffle
      loop do
        sync :master
        stop if terminate
        tick
        midi notes.look
      end
    ensure
      midi 20
      sleep 1
    end
  end
end

# track 6 (baritone)
uncomment do
  in_thread do
    with_bpm 120 do
      notes = (range 37, 48).shuffle
      loop do
        sync :master
        stop if terminate
        tick
        midi notes.look
      end
    ensure
      midi 48
      sleep 1
    end
  end
end
