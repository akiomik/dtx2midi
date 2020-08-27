{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI
  ( fromDTXFile,
    toMIDIFile,
    dtxToMIDI,
    -- for testing
    keyCompletion,
    objectCompletion,
  )
where

import DTX2MIDI.DTX (DTX (..))
import qualified DTX2MIDI.DTX as DTX
import DTX2MIDI.DTX.Parser
import DTX2MIDI.MIDI
import qualified DTX2MIDI.MIDI as MIDI
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

-- | DTXファイルの読み込み
fromDTXFile :: FilePath -> IO (DTX)
fromDTXFile = readFile

-- | MIDIデータをMIDIファイルとして保存
toMIDIFile :: FilePath -> MIDI -> IO ()
toMIDIFile = exportMidiFile

musicToMIDI :: (Music.ToMusic1 a) => Music a -> MIDI
musicToMIDI = toMidi . perform

-- | DTXデータをMIDIデータに変換
--   NOTE: changeTempoは音価が変わるだけでBPM自体は変化しないため、
--         global tempo (bpm 120) を無視して上書き
dtxToMIDI :: DTX -> IO (MIDI)
dtxToMIDI dtx = do
  let grouped = groupBy isSameMeasure $ completedObjects $ DTX.objects dtx
  let music = Music.line1 $ map objectsToMIDIMeasure grouped
  return $ case DTX.baseBPM dtx of
    Nothing -> musicToMIDI music
    Just b -> MIDI.updateInitialTempo (MIDI.bpmToTempo b) $ musicToMIDI music
  where
    isSameMeasure = (==) `on` DTX.objectKey

-- | 同一小節のDTXオブジェクトをMIDIの1小節に変換
objectsToMIDIMeasure :: [DTX.Object] -> Music Music.Pitch
objectsToMIDIMeasure =
  Music.chord1 . mapMaybe toNotes
  where
    toNotes = objectValueToMIDINotes . DTX.objectValue

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
  map (T.pack . format) [0 .. ((read $ T.unpack $ last ts) :: Int)]
  where
    format a
      | a < 10 = "00" ++ show a
      | a < 100 = "0" ++ show a
      | otherwise = show a

-- | 不足している小節を補完したDTXオブジェクトを返す
completedObjects :: [DTX.Object] -> [DTX.Object]
completedObjects objects =
  sortBy (compare `on` DTX.objectKey) $ objects ++ completion
  where
    completion = objectCompletion completionKeys
    completionKeys = map DTX.objectKey $ filter DTX.isNoteObject objects
