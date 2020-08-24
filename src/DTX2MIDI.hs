{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI
  ( fromFile,
    toFile,
    toMIDI,
    -- for testing
    keyCompletion,
    objectCompletion,
    toTempo,
  )
where

import Codec.Midi (Midi (..))
import qualified Codec.Midi as Midi
import DTX2MIDI.DTX (DTX (..))
import qualified DTX2MIDI.DTX as DTX
import DTX2MIDI.DTX.Parser
import Data.Function (on)
import Data.List (groupBy, sortBy, (\\))
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Text as T
import Euterpea.IO.MIDI.ExportMidiFile (exportMidiFile)
import Euterpea.IO.MIDI.MEvent (perform)
import Euterpea.IO.MIDI.ToMidi (toMidi)
import Euterpea.Music (Music (..))
import qualified Euterpea.Music as Music
import Prelude hiding (readFile)

type DrumSound = Music.Dur -> Music Music.Pitch

-- | MIDIデータからMIDIファイルへ変換
toFile :: FilePath -> Midi -> IO ()
toFile = exportMidiFile

-- | DTXファイルからMIDIデータへ変換
fromFile :: FilePath -> IO (Midi)
fromFile fp = do
  dtx <- readFile fp
  toMIDI dtx

-- | Euterpeaの固定テンポ(bpm = 120, tempo = 500000)を上書きする
updateFirstTempo :: Midi.Tempo -> Midi -> Midi
updateFirstTempo tempo midi =
  Midi
    { Midi.fileType = Midi.fileType midi,
      Midi.timeDiv = Midi.timeDiv midi,
      Midi.tracks = mapTrack update $ Midi.tracks midi
    }
  where
    mapTrack f tracks = map (\track -> map f track) tracks
    update (0, Midi.TempoChange 500000) = (0, Midi.TempoChange tempo)
    update t = t

-- bpm (beat/minute) を tempo (μs/beat) に変換する
toTempo :: Double -> Midi.Tempo
toTempo bpm = round $ 1000000 / (bpm / 60)

-- | DTXデータをMIDIデータに変換
--   NOTE: changeTempoは音価が変わるだけでBPM自体は変化しないため、
--         global tempo (bpm 120) を無視して上書き
toMIDI :: DTX -> IO (Midi)
toMIDI dtx = do
  let group = groupBy sameKey filteredObjects
  let midi = Music.line1 $ map toMeasure group
  return $ case DTX.bpm dtx of
    Nothing -> toMidi $ perform midi
    Just b -> updateFirstTempo (toTempo b) $ toMidi $ perform midi
  where
    toNotes o = objectValueToMIDINotes $ DTX.objectValue o
    toMeasure = Music.chord1 . mapMaybe toNotes
    sameKey = (==) `on` DTX.objectKey
    objects = DTX.objects dtx
    keys = map DTX.objectKey $ filter DTX.isNoteObject objects
    completion = objectCompletion keys
    fullObjects = sortBy (compare `on` DTX.objectKey) $ objects ++ completion
    filteredObjects = filter (\o -> DTX.objectKey o /= "000") fullObjects -- BGM用の小節000は無視

-- | DTXのオブジェクトからドラム音源へマッピング
-- TODO: ボリューム等の対応
-- TODO: BPMの変更などのコントロール系の処理
objectValueToMIDINotes :: DTX.ObjectValue -> Maybe (Music Music.Pitch)
objectValueToMIDINotes (DTX.HiHatClose notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.ClosedHiHat) notes
objectValueToMIDINotes (DTX.Snare notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.AcousticSnare) notes
objectValueToMIDINotes (DTX.BassDrum notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.AcousticBassDrum) notes
objectValueToMIDINotes (DTX.HighTom notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.HighTom) notes
objectValueToMIDINotes (DTX.LowTom notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.LowTom) notes
objectValueToMIDINotes (DTX.Cymbal notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.CrashCymbal1) notes
objectValueToMIDINotes (DTX.FloorTom notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.LowFloorTom) notes
objectValueToMIDINotes (DTX.HiHatOpen notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.OpenHiHat) notes
objectValueToMIDINotes (DTX.RideCymbal notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.RideCymbal1) notes
objectValueToMIDINotes (DTX.LeftCymbal notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.CrashCymbal2) notes
objectValueToMIDINotes (DTX.LeftPedal notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.PedalHiHat) notes
objectValueToMIDINotes (DTX.LeftBassDrum notes) = Just $ dtxNotesToMIDINotes (Music.perc Music.AcousticBassDrum) notes
objectValueToMIDINotes _ = Nothing

dtxNotesToMIDINotes :: DrumSound -> [DTX.Note] -> Music Music.Pitch
dtxNotesToMIDINotes drum notes =
  Music.line $ map (\note -> dtxNoteToMIDINote drum note dur) notes
  where
    dur = toDur notes
    toDur notes = 1 % (toInteger $ length notes)

dtxNoteToMIDINote :: DrumSound -> DTX.Note -> Music.Dur -> Music Music.Pitch
dtxNoteToMIDINote _ "00" dur = Music.rest dur
dtxNoteToMIDINote drum _ dur = drum dur

-- | 疎になっている小節のオブジェクトを休符で補完する
objectCompletion :: [DTX.Key] -> [DTX.Object]
objectCompletion keys =
  map (\key -> DTX.Object key (DTX.HiHatClose ["00"])) $ keyCompletion keys \\ keys

-- | 疎になっている小節のキーを補完する
keyCompletion :: [DTX.Key] -> [DTX.Key]
keyCompletion ts =
  map (T.pack . format) [1 .. ((read $ T.unpack $ last ts) :: Int)]
  where
    format a
      | a < 10 = "00" ++ show a
      | a < 100 = "0" ++ show a
      | otherwise = show a
