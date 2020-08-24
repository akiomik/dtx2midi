{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI
  ( fromFile,
    toFile,
    toMIDI,
    -- for testing
    bpm,
    keyCompletion,
    objectCompletion,
    toTempo,
  )
where

import Codec.Midi (Midi (..))
import qualified Codec.Midi as Midi
import qualified DTX2MIDI.DTX as DTX
import DTX2MIDI.DTX.Parser
import Data.Function (on)
import Data.List (groupBy, sortBy, (\\))
import Data.Maybe (listToMaybe, maybeToList)
import Data.Ratio ((%))
import qualified Data.Text as T
import Euterpea.IO.MIDI.ExportMidiFile (exportMidiFile)
import Euterpea.IO.MIDI.MEvent (perform)
import Euterpea.IO.MIDI.ToMidi (toMidi)
import Euterpea.Music (Music (..))
import qualified Euterpea.Music as Music
import Prelude hiding (readFile)

type DTX = [DTX.Line]

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

-- | BPMを取得
bpm :: DTX -> Maybe Double
bpm dtx =
  fmap readValue $ listToMaybe $ filter isBPM headers
  where
    headers = (maybeToList . DTX.header) =<< dtx
    readValue :: DTX.Header -> Double
    readValue = read . T.unpack . DTX.headerValue
    isBPM = (== "BPM") . DTX.headerKey

-- bpm (beat/minute) を tempo (μs/beat) に変換する
toTempo :: Double -> Midi.Tempo
toTempo bpm = round $ 1000000 / (bpm / 60)

-- | DTXデータをMIDIデータに変換
--   NOTE: changeTempoは音価が変わるだけでBPM自体は変化しないため、
--         global tempo (bpm 120) を無視して上書き
toMIDI :: DTX -> IO (Midi)
toMIDI lines = do
  let group = groupBy sameKey filteredObjects
  let midi = Music.line1 $ map toMeasure group
  return $ case bpm lines of
    Nothing -> toMidi $ perform midi
    Just b -> updateFirstTempo (toTempo b) $ toMidi $ perform midi
  where
    toNote o = valueToMIDINote (chanToDrum $ DTX.objectChannel o) $ DTX.objectValue o
    toMeasure = Music.chord1 . map toNote
    sameKey = (==) `on` DTX.objectKey
    objects = (maybeToList . DTX.object) =<< lines
    keys = map DTX.objectKey objects
    completion = objectCompletion keys
    fullObjects = sortBy (compare `on` DTX.objectKey) $ objects ++ completion
    filteredObjects = filter (\o -> DTX.objectKey o /= "000") fullObjects -- BGM用の小節000は無視

-- | チャンネルからドラム音源へマッピング
-- TODO: ボリューム等の対応
-- TODO: BPMの変更などのコントロール系の処理
chanToDrum :: T.Text -> DrumSound
chanToDrum "11" d = Music.perc Music.ClosedHiHat d
chanToDrum "12" d = Music.perc Music.AcousticSnare d
chanToDrum "13" d = Music.perc Music.AcousticBassDrum d
chanToDrum "14" d = Music.perc Music.HighTom d
chanToDrum "15" d = Music.perc Music.LowTom d
chanToDrum "16" d = Music.perc Music.CrashCymbal1 d
chanToDrum "17" d = Music.perc Music.LowFloorTom d
chanToDrum "18" d = Music.perc Music.OpenHiHat d
chanToDrum "19" d = Music.perc Music.RideCymbal1 d
chanToDrum "1A" d = Music.perc Music.CrashCymbal2 d
chanToDrum "1B" d = Music.perc Music.PedalHiHat d -- ペダル？
chanToDrum "1C" d = Music.perc Music.AcousticBassDrum d -- ツインペダル？
chanToDrum _ d = Music.rest d

-- | ドラム音源 drum と オブジェクト値 notes をmidiデータに変換する
-- TODO: 変拍子対応
valueToMIDINote :: DrumSound -> [DTX.Note] -> Music Music.Pitch
valueToMIDINote drum notes =
  Music.line $ map toMIDINote notes
  where
    len = toInteger $ length notes
    d = 1 % len
    toMIDINote "00" = Music.rest d
    toMIDINote _ = drum d

-- | 疎になっている小節のオブジェクトを休符で補完する
objectCompletion :: [DTX.Key] -> [DTX.Object]
objectCompletion keys =
  map (\key -> DTX.Object key "11" ["00"]) $ keyCompletion keys \\ keys

-- | 疎になっている小節のキーを補完する
keyCompletion :: [DTX.Key] -> [DTX.Key]
keyCompletion ts =
  map (T.pack . format) [1 .. ((read $ T.unpack $ last ts) :: Int)]
  where
    format a
      | a < 10 = "00" ++ show a
      | a < 100 = "0" ++ show a
      | otherwise = show a
