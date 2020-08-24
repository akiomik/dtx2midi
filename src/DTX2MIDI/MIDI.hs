{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.MIDI
  ( MIDI (..),
    updateInitialTempo,
    bpmToTempo,
  )
where

import Codec.Midi (Midi (..))
import qualified Codec.Midi as Midi

type MIDI = Midi

type BPM = Double

-- | Euterpeaの固定テンポ(bpm = 120, tempo = 500000)を上書きする
updateInitialTempo :: Midi.Tempo -> MIDI -> MIDI
updateInitialTempo tempo midi =
  midi {Midi.tracks = mapTrack update $ Midi.tracks midi}
  where
    mapTrack f tracks = map (\track -> map f track) tracks
    update (0, Midi.TempoChange 500000) = (0, Midi.TempoChange tempo)
    update t = t

-- bpm (beat/minute) を tempo (μs/beat) に変換する
bpmToTempo :: BPM -> Midi.Tempo
bpmToTempo bpm = round $ 1000000 / (bpm / 60)
