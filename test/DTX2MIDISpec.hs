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
        `shouldBe` ( [ Object "002" $ HiHatClose ["00"],
                       Object "003" $ HiHatClose ["00"],
                       Object "004" $ HiHatClose ["00"]
                     ]
                   )

  describe "dtxToMIDI" $ do
    it "returns midi with specified bpm when the header has a BPM" $ do
      -- input
      let dtx =
            ( [ LineHeader $ Header "BPM" "" "180",
                LineObject $ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
                LineObject $ Object "001" $ Snare ["00", "02", "00", "02"],
                LineObject $ Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
              ]
            )

      -- expected
      let chan = 9
      let vel = 127
      let bd = 35
      let sd = 38
      let hh = 42
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 333333),
                (0, Midi.NoteOn chan bd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan sd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan sd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan bd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan bd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan sd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan sd vel),
                (0, Midi.NoteOff chan hh vel)
              ]
            ]
      let expected =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = tracks
              }

      midi <- dtxToMIDI dtx
      midi `shouldBe` (expected)

    it "returns midi with default bpm when the header has not a BPM" $ do
      -- input
      let dtx =
            ( [ LineObject $ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
                LineObject $ Object "001" $ Snare ["00", "02", "00", "02"],
                LineObject $ Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
              ]
            )

      -- expected
      let chan = 9
      let vel = 127
      let bd = 35
      let sd = 38
      let hh = 42
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 500000),
                (0, Midi.NoteOn chan bd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan sd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan sd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan bd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan bd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan sd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan sd vel),
                (0, Midi.NoteOff chan hh vel)
              ]
            ]
      let expected =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = tracks
              }

      midi <- dtxToMIDI dtx
      midi `shouldBe` (expected)

    it "returns midi with the missing measure completed by rests" $ do
      -- input
      let dtx =
            ( [ LineObject $ Object "001" $ BassDrum ["03", "03", "03", "03"],
                LineObject $ Object "003" $ BassDrum ["03", "03", "03", "03"]
              ]
            )

      -- expected
      let chan = 9
      let vel = 127
      let bd = 35
      let hh = 42
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 500000),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (96 * 4, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel)
              ]
            ]
      let expected =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = tracks
              }

      midi <- dtxToMIDI dtx
      midi `shouldBe` (expected)

    it "returns midi with the unsupported event measure completed by rests" $ do
      -- input
      let dtx =
            ( [ LineObject $ Object "001" $ BassDrum ["03", "03", "03", "03"],
                LineObject $ Object "002" $ UnsupportedEvent "02" "0.5",
                LineObject $ Object "003" $ BassDrum ["03", "03", "03", "03"]
              ]
            )

      -- expected
      let chan = 9
      let vel = 127
      let bd = 35
      let hh = 42
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 500000),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (96 * 4, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOn chan bd vel),
                (96, Midi.NoteOff chan bd vel)
              ]
            ]
      let expected =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = tracks
              }

      midi <- dtxToMIDI dtx
      midi `shouldBe` (expected)
