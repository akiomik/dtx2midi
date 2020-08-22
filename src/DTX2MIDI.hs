{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI
    (
      fromFile
    , toFile
    , toMIDI

    -- for testing
    , bpm
    , keyCompletion
    , objectCompletion
    , toTempo
    ) where

import qualified Haskore.Basic.Duration as Duration
import qualified Haskore.Composition.Drum as Drum
import Haskore.Basic.Duration ((%+))
import Haskore.Interface.MIDI.Render (generalMidi)
import qualified Haskore.Music as Music
import Haskore.Music.GeneralMIDI (Drum, line, (=:=), (+:+))
import qualified Haskore.Music.Rhythmic as Rhythmic
import qualified Sound.MIDI.File.Save as SaveMidi
import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event as MIDIEvent
import qualified Sound.MIDI.File as MIDIFile
import qualified Data.EventList.Relative.TimeBody as EventList
import Data.EventList.Relative.MixedBody ((/.), (./), )

import Control.Monad (join)
import Data.Function (on)
import Data.List (partition, groupBy, (\\), sortBy)
import Data.Maybe (maybeToList, listToMaybe)
import qualified Data.Text as T
import DTX
import DTX.Parse
import Prelude hiding (readFile)

type DTX = [Line]
type MIDI instr = Music.T (Rhythmic.Note Drum instr)
type DrumSound instr = Duration.T -> Rhythmic.T Drum instr

-- | MIDIデータからMIDIファイルへ変換
toFile :: FilePath -> MIDIFile.T -> IO ()
toFile filename midi = SaveMidi.toFile filename midi

-- | DTXファイルからMIDIデータへ変換
fromFile :: FilePath -> IO (MIDIFile.T)
fromFile fp = do
    dtx <- readFile fp
    toMIDI dtx

-- | SetTempoイベントを上書きする
mapSetTempo :: (MIDIFile.Tempo -> MIDIFile.Tempo) -> MIDIFile.T -> MIDIFile.T
mapSetTempo f midi =
    MIDIFile.mapTrack (EventList.mapBody ff) midi
  where
    ff :: MIDIEvent.T -> MIDIEvent.T
    ff (MIDIEvent.MetaEvent (MetaEvent.SetTempo t)) = MIDIEvent.MetaEvent (MetaEvent.SetTempo $ f t)
    ff e = e

-- | BPMを取得
bpm :: DTX -> Maybe Double
bpm dtx =
    fmap readValue $ listToMaybe $ filter isBPM headers
  where
    headers = (maybeToList . header) =<< dtx
    readValue :: Header -> Double
    readValue = read . T.unpack . headerValue
    isBPM = (== "BPM") . headerKey

-- bpm (beat/minute) を tempo (μs/beat) に変換する
toTempo :: Double -> MIDIFile.Tempo
toTempo bpm = MIDIFile.toTempo $ round $ 1000000 / (bpm / 60)

-- | DTXデータをMIDIデータに変換
--   NOTE: changeTempoは音価が変わるだけでBPM自体は変化しないため、
--         global tempo (bpm 120) を無視して上書き
toMIDI :: DTX -> IO (MIDIFile.T)
toMIDI lines = do
    let group = groupBy sameKey filteredObjects
    let midi = foldl1 (+:+) $ map toMeasure group
    return $ case bpm lines of
        Nothing -> generalMidi midi
        Just b -> mapSetTempo (\t -> toTempo b) $ generalMidi $ Music.changeTempo 2 midi
  where
    toNote o = valueToMIDINote (chanToDrum $ objectChannel o) $ objectValue o
    toMeasure = foldl1 (=:=) . map toNote
    sameKey = (==) `on` objectKey
    objects = (maybeToList . object) =<< lines
    keys = map objectKey objects
    completion = objectCompletion keys
    fullObjects = sortBy (compare `on` objectKey) $ objects ++ completion
    filteredObjects = filter (\o -> objectKey o /= "000") fullObjects -- BGM用の小節000は無視

-- | チャンネルからドラム音源へマッピング
-- TODO: ボリューム等の対応
-- TODO: BPMの変更などのコントロール系の処理
chanToDrum :: T.Text -> DrumSound instr
chanToDrum "11" d = Drum.toMusicDefaultAttr Drum.ClosedHiHat d
chanToDrum "12" d = Drum.toMusicDefaultAttr Drum.AcousticSnare d
chanToDrum "13" d = Drum.toMusicDefaultAttr Drum.AcousticBassDrum d
chanToDrum "14" d = Drum.toMusicDefaultAttr Drum.HighTom d
chanToDrum "15" d = Drum.toMusicDefaultAttr Drum.LowTom d
chanToDrum "16" d = Drum.toMusicDefaultAttr Drum.CrashCymbal1 d
chanToDrum "17" d = Drum.toMusicDefaultAttr Drum.LowFloorTom d
chanToDrum "18" d = Drum.toMusicDefaultAttr Drum.OpenHiHat d
chanToDrum "19" d = Drum.toMusicDefaultAttr Drum.RideCymbal1 d
chanToDrum "1A" d = Drum.toMusicDefaultAttr Drum.CrashCymbal2 d
chanToDrum "1B" d = Drum.toMusicDefaultAttr Drum.PedalHiHat d -- ペダル？
chanToDrum "1C" d = Drum.toMusicDefaultAttr Drum.AcousticBassDrum d -- ツインペダル？
chanToDrum  _   d = Music.rest d

-- | ドラム音源 drum と オブジェクト値 t をmidiデータに変換する
-- TODO: 変拍子対応
valueToMIDINote :: DrumSound instr -> [Note] -> MIDI instr
valueToMIDINote drum notes =
    line $ map toMIDINote notes
  where
    len = toInteger $ length notes
    d = 1 %+ len
    toMIDINote "00" = Music.rest d
    toMIDINote _ = drum d

-- | 疎になっている小節のオブジェクトを休符で補完する
objectCompletion :: [T.Text] -> [Object]
objectCompletion keys =
    map (\key -> Object key "11" ["00"]) $ keyCompletion keys \\ keys

-- | 疎になっている小節のキーを補完する
keyCompletion :: [T.Text] -> [T.Text]
keyCompletion ts =
    map (T.pack . format) [1..((read $ T.unpack $ last ts) :: Int)]
  where
    format a
        | a < 10 = "00" ++ show a
        | a < 100 = "0" ++ show a
        | otherwise = show a
