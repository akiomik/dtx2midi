{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDISpec where

import Codec.Midi (Midi (..))
import qualified Codec.Midi as Midi
import DTX2MIDI
import DTX2MIDI.DTX
import DTX2MIDI.DTX.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "keyCompletion" $ do
    it "returns filled measure ids" $ do
      keyCompletion ["001", "005"] `shouldBe` (["001", "002", "003", "004", "005"])

  describe "objectCompletion" $ do
    it "returns missing measure objects that are filled with empty hi-hats" $ do
      objectCompletion ["001", "005"]
        `shouldBe` ( [ Object "002" "11" ["00"],
                       Object "003" "11" ["00"],
                       Object "004" "11" ["00"]
                     ]
                   )

  describe "toTempo" $ do
    it "returns tempo which has a specified bpm" $ do
      toTempo 120 `shouldBe` 500000
      toTempo 140 `shouldBe` 428571

  describe "toMIDI" $ do
    it "returns midi with specified bpm when the header has a BPM" $ do
      -- input
      let dtx =
            ( [ LineHeader $ Header "BPM" "" "180",
                LineObject $ Object "001" "11" ["01", "01", "01", "01", "01", "01", "01", "01"],
                LineObject $ Object "001" "12" ["00", "02", "00", "02"],
                LineObject $ Object "001" "13" ["03", "00", "00", "00", "03", "03", "00", "00"]
              ]
            )

      -- expected
      let chan = 9
      let vel = 127
      let bd = 35
      let sd = 38
      let hh = 42
      let tracks =
            [ [ (0, Midi.ProgramChange {Midi.channel = chan, Midi.preset = 0}),
                (0, Midi.TempoChange 333333),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel})
              ]
            ]
      let expected =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = tracks
              }

      midi <- toMIDI dtx
      midi `shouldBe` (expected)

    it "returns midi with default bpm when the header has not a BPM" $ do
      -- input
      let dtx =
            ( [ LineObject $ Object "001" "11" ["01", "01", "01", "01", "01", "01", "01", "01"],
                LineObject $ Object "001" "12" ["00", "02", "00", "02"],
                LineObject $ Object "001" "13" ["03", "00", "00", "00", "03", "03", "00", "00"]
              ]
            )

      -- expected
      let chan = 9
      let vel = 127
      let bd = 35
      let sd = 38
      let hh = 42
      let tracks =
            [ [ (0, Midi.ProgramChange {Midi.channel = chan, Midi.preset = 0}),
                (0, Midi.TempoChange 500000),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = bd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (0, Midi.NoteOn {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel}),
                (48, Midi.NoteOff {Midi.channel = chan, Midi.key = sd, Midi.velocity = vel}),
                (0, Midi.NoteOff {Midi.channel = chan, Midi.key = hh, Midi.velocity = vel})
              ]
            ]
      let expected =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = tracks
              }

      midi <- toMIDI dtx
      midi `shouldBe` (expected)
