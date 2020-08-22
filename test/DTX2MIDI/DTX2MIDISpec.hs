{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.DTX2MIDISpec where

import Test.Hspec

import qualified Sound.MIDI.File     as MIDIFile
import Sound.MIDI.File.Event            (T(MIDIEvent, MetaEvent))
import Sound.MIDI.File.Event.Meta       (T(SetTempo))
import Sound.MIDI.Message.Channel       (T(Cons), Channel, Body(Voice), toChannel)
import Sound.MIDI.Message.Channel.Voice (T(NoteOn, NoteOff, ProgramChange), toVelocity, toPitch, toProgram)
import qualified Data.EventList.Relative.TimeBody as EventList
import Data.EventList.Relative.MixedBody ((/.), (./))

import DTX
import DTX.Parser
import DTX2MIDI

spec :: Spec
spec = do
  describe "bpm" $ do
    it "returns bpm when BPM is defined in the headers" $ do
      let dtx = [ LineHeader $ Header "TITLE" "" "foo"
                , LineHeader $ Header "BPM" "" "100" ]
      bpm dtx `shouldBe` (Just 100)

    it "returns Nothing when BPM is not defined in the headers" $ do
      let dtx = [LineHeader $ Header "TITLE" "" "foo"]
      bpm dtx `shouldBe` (Nothing)

  describe "keyCompletion" $ do
    it "returns filled measure ids" $ do
      keyCompletion ["001", "005"] `shouldBe` (["001", "002", "003", "004", "005"])

  describe "objectCompletion" $ do
    it "returns missing measure objects that are filled with empty hi-hats" $ do
      objectCompletion ["001", "005"] `shouldBe` ([ Object "002" "11" ["00"]
                                                  , Object "003" "11" ["00"]
                                                  , Object "004" "11" ["00"] ])

  describe "toTempo" $ do
    it "returns tempo which has a specified bpm" $ do
      toTempo 120 `shouldBe` 500000
      toTempo 140 `shouldBe` 428571

  describe "toMIDI" $ do
    it "returns midi with specified bpm when the header has a BPM" $ do
      -- input
      let dtx = ([ LineHeader $ Header "BPM" "" "180"
                 , LineObject $ Object "001" "11" ["01", "01", "01", "01", "01", "01", "01", "01"]
                 , LineObject $ Object "001" "12" ["00", "02", "00", "02"]
                 , LineObject $ Object "001" "13" ["03", "00", "00", "00", "03", "03", "00", "00"] ])

      -- expected
      let chan = toChannel 9
      let vel = toVelocity 64
      let bd = toPitch 35
      let sd = toPitch 38
      let hh = toPitch 42
      let events = [ 0  /. MetaEvent (SetTempo 333333) ./
                     0  /. MIDIEvent (Cons chan (Voice (ProgramChange (toProgram 0)))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     48 /. MIDIEvent (Cons chan (Voice (NoteOff sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     EventList.empty]
      let expected = MIDIFile.Cons MIDIFile.Parallel (MIDIFile.Ticks 96) events

      midi <- toMIDI dtx
      midi `shouldBe` (expected)

    it "returns midi with default bpm when the header has not a BPM" $ do
      -- input
      let dtx = ([ LineObject $ Object "001" "11" ["01", "01", "01", "01", "01", "01", "01", "01"]
                 , LineObject $ Object "001" "12" ["00", "02", "00", "02"]
                 , LineObject $ Object "001" "13" ["03", "00", "00", "00", "03", "03", "00", "00"] ])

      -- expected
      let chan = toChannel 9
      let vel = toVelocity 64
      let bd = toPitch 35
      let sd = toPitch 38
      let hh = toPitch 42
      let events = [ 0  /. MetaEvent (SetTempo 500000) ./
                     0  /. MIDIEvent (Cons chan (Voice (ProgramChange (toProgram 0)))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff bd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOn  hh vel))) ./
                     96 /. MIDIEvent (Cons chan (Voice (NoteOff sd vel))) ./
                     0  /. MIDIEvent (Cons chan (Voice (NoteOff hh vel))) ./
                     EventList.empty]
      let expected = MIDIFile.Cons MIDIFile.Parallel (MIDIFile.Ticks 96) events

      midi <- toMIDI dtx
      midi `shouldBe` (expected)
