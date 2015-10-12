module DTX2MIDI
    (
      fromFile
    , toFile
    , toMIDI
    ) where

import qualified Haskore.Basic.Duration as Duration
import qualified Haskore.Composition.Drum as Drum
import Haskore.Basic.Duration ((%+))
import Haskore.Interface.MIDI.Render (fileFromGeneralMIDIMusic)
import qualified Haskore.Music as Music
import Haskore.Music.GeneralMIDI (Drum, line, (=:=), (+:+))
import qualified Haskore.Music.GeneralMIDI as GM
import qualified Haskore.Music.Rhythmic as Rhythmic

import Control.Monad (join)
import Data.Function (on)
import Data.List (partition, groupBy)
import Data.Maybe (maybeToList, listToMaybe)
import qualified Data.Text as T
import DTX.Parse
import Prelude hiding (readFile)

type DTX = [Line]
type MIDI instr = Music.T (Rhythmic.Note Drum instr)
type DrumSound instr = Duration.T -> Rhythmic.T Drum instr

-- | MIDIデータからMIDIファイルへ変換
toFile :: FilePath -> GM.T -> IO ()
toFile = fileFromGeneralMIDIMusic

-- | DTXファイルからMIDIデータへ変換
fromFile :: FilePath -> IO (MIDI instr)
fromFile fp = do
    dtx <- readFile fp
    toMIDI dtx

-- | BPMを取得
bpm :: DTX -> Maybe Double
bpm dtx =
    fmap readValue $ listToMaybe $ filter isBPM headers
  where
    headers = (maybeToList . header) =<< dtx
    readValue :: Header -> Double
    readValue = read . T.unpack . headerValue
    isBPM = (== "BPM") . headerKey

-- | DTXデータをMIDIデータに変換
-- FIXME: 小節がまるまる休みのところをカウントできていない
toMIDI :: DTX -> IO (MIDI instr)
toMIDI lines = do
    let group = groupBy sameKey objects
    let midi = foldl1 (+:+) $ map toMeasure group
    return $ case bpm lines of
        Nothing -> midi
        Just b -> Music.changeTempo (realToFrac b / 60) midi
  where
    toNote o = valueToNote (chanToDrum $ objectChannel o) $ objectValue o
    toMeasure = foldl1 (=:=) . map toNote
    sameKey = (==) `on` objectKey
    objects = (maybeToList . object) =<< lines

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
valueToNote :: DrumSound instr -> T.Text -> MIDI instr
valueToNote drum t =
    line $ map note objs
  where
    objs = parseObjectValue t
    len = toInteger $ length objs
    d = 1 %+ len
    note "00" = Music.rest d
    note _ = drum d

-- 値を2つずつにまとめて、オブジェクト値をパースする
-- eg. "010203" -> ["01", "02", "03"]
parseObjectValue :: T.Text -> [T.Text]
parseObjectValue =
    map parse . uncurry zip . oddEven . T.unpack
  where
    oddEven = partition (odd . fst) . zip [0..] 
    parse ((_, a), (_, b)) = T.pack $ a:[b]

