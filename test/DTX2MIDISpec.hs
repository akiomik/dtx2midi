{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Reduce duplication" -}

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
      keyCompletion ["001", "005"] `shouldBe` ["000", "001", "002", "003", "004", "005"]

  describe "objectCompletion" $ do
    it "returns missing measure objects that are filled with empty hi-hats" $ do
      objectCompletion ["001", "005"]
        `shouldBe` [ Object "000" $ HiHatClose ["00"],
                     Object "002" $ HiHatClose ["00"],
                     Object "003" $ HiHatClose ["00"],
                     Object "004" $ HiHatClose ["00"]
                   ]

  describe "completedNoteObjects" $ do
    it "returns note objects that are filled with empty hi-hats" $ do
      let input =
            [ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              Object "001" $ Snare ["00", "02", "00", "02"],
              Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"],
              Object "002" $ UnsupportedEvent "61" "61",
              Object "004" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              Object "004" $ Snare ["00", "02", "00", "02"],
              Object "004" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"],
              Object "005" $ UnsupportedEvent "61" "61"
            ]
      let expected =
            [ Object "000" $ HiHatClose ["00"],
              Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              Object "001" $ Snare ["00", "02", "00", "02"],
              Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"],
              Object "002" $ HiHatClose ["00"],
              Object "003" $ HiHatClose ["00"],
              Object "004" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              Object "004" $ Snare ["00", "02", "00", "02"],
              Object "004" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
            ]
      completedNoteObjects input `shouldBe` expected

  describe "dtxToMIDI" $ do
    let chan = 9
    let vel = 127
    let bd = 35
    let sd = 38
    let hh = 42
    let genMidi = \tracks ->
          Midi
            { Midi.fileType = Midi.SingleTrack,
              Midi.timeDiv = Midi.TicksPerBeat 96,
              Midi.tracks = tracks
            }

    it "returns midi with specified bpm when the header has a BPM" $ do
      -- input
      let dtx =
            [ LineHeader $ Header "BPM" "" "180",
              LineObject $ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              LineObject $ Object "001" $ Snare ["00", "02", "00", "02"],
              LineObject $ Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
            ]

      -- expected
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 333333),
                (96 * 4, Midi.NoteOn chan bd vel),
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

      midi <- dtxToMIDI dtx
      midi `shouldBe` genMidi tracks

    it "returns midi with default bpm when the header has not a BPM" $ do
      -- input
      let dtx =
            [ LineObject $ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              LineObject $ Object "001" $ Snare ["00", "02", "00", "02"],
              LineObject $ Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
            ]

      -- expected
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 500000),
                (96 * 4, Midi.NoteOn chan bd vel),
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

      midi <- dtxToMIDI dtx
      midi `shouldBe` genMidi tracks

    it "returns midi with the missing measure completed by rests" $ do
      -- input
      let dtx =
            [ LineObject $ Object "001" $ BassDrum ["03", "03", "03", "03"],
              LineObject $ Object "003" $ BassDrum ["03", "03", "03", "03"]
            ]

      -- expected
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 500000),
                (96 * 4, Midi.NoteOn chan bd vel),
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

      midi <- dtxToMIDI dtx
      midi `shouldBe` genMidi tracks

    it "returns midi with the unsupported event measure completed by rests" $ do
      -- input
      let dtx =
            [ LineObject $ Object "001" $ BassDrum ["03", "03", "03", "03"],
              LineObject $ Object "002" $ UnsupportedEvent "02" "0.5",
              LineObject $ Object "003" $ BassDrum ["03", "03", "03", "03"]
            ]

      -- expected
      let tracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 500000),
                (96 * 4, Midi.NoteOn chan bd vel),
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

      midi <- dtxToMIDI dtx
      midi `shouldBe` genMidi tracks
