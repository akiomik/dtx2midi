{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.MIDISpec where

import Codec.Midi (Midi (..))
import qualified Codec.Midi as Midi
import DTX2MIDI.MIDI
import Test.Hspec

spec :: Spec
spec = do
  describe "bpmToTempo" $ do
    it "returns tempo which has a specified bpm" $ do
      bpmToTempo 120 `shouldBe` 500000
      bpmToTempo 140 `shouldBe` 428571

  describe "updateInitialTempo" $ do
    it "returns midi which has a specified initial tempo" $ do
      let chan = 9
      let vel = 127
      let bd = 35
      let sd = 38
      let hh = 42
      let expectedTracks =
            [ [ (0, Midi.ProgramChange chan 0),
                (0, Midi.TempoChange 250000),
                (0, Midi.NoteOn chan bd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan bd vel),
                (0, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (0, Midi.NoteOn chan sd vel),
                (0, Midi.NoteOn chan hh vel),
                (48, Midi.NoteOff chan hh vel),
                (48, Midi.NoteOff chan sd vel)
              ]
            ]
      let expectedMidi =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = expectedTracks
              }

      let inputTracks =
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
                (48, Midi.NoteOff chan sd vel)
              ]
            ]
      let inputMidi =
            Midi
              { Midi.fileType = Midi.SingleTrack,
                Midi.timeDiv = Midi.TicksPerBeat 96,
                Midi.tracks = inputTracks
              }

      updateInitialTempo 250000 inputMidi `shouldBe` expectedMidi
