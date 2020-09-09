
module DwCSV.CSV
    (
      outputCSV
    , namedColumns
    , pairsInColumns
    , tuple3InColumns
    , tuple4InColumns
    , tuple5InColumns
    , tuple6InColumns
    , timestampWithList
    , list2Columns
    , transpose
    , textify
    , Range
    )
where

import Control.Applicative (ZipList (..))
import Data.Text (Text, intercalate, pack)
import qualified Data.Text.IO as TIO

import System.IO (Handle)

-- the type of a cell
type ValTy = Text

type Column = [ValTy]
-- type Row = [ValTy]

type Range = [Column]

outputCSV :: Handle -> Range -> IO ()
outputCSV _h [] = pure ()
outputCSV h (r : rs) = do
    TIO.hPutStrLn h $ intercalate (","::Text) r
    outputCSV h rs

namedColumns :: [([Text], Range)] -> Range
namedColumns ls =
    enter_ranges ls (0,[[]])
  where
    enter_ranges :: [([Text], Range)] -> (Int, Range) -> Range
    enter_ranges [] (_w,acc) = acc
    enter_ranges ((headers, range):r) (w,acc) =
      let w2 = length headers
          newcols = headers : range
          rng2 = append_cols (w,acc) (w2,newcols) []
      in enter_ranges r (w+w2, rng2)
    append_cols (_,[]) (_,[]) acc = reverse acc   --termination
    append_cols (w1,[]) (w2, b : bs) acc = append_cols (w1,[]) (w2, bs) $ (replicate w1 (pack " ") ++ b) : acc
    append_cols (w1, a : as) (w2, []) acc = append_cols (w1,as) (w2,[]) $ (a ++ replicate w2 (" "::Text)) : acc
    append_cols (w1, a : as) (w2, b : bs) acc = append_cols (w1,as) (w2,bs) $ (a ++ b) : acc


pairsInColumns :: (Show a, Show b) => [(a,b)] -> Range
pairsInColumns = map (\(a,b) -> [textify a, textify b])

tuple3InColumns :: (Show a, Show b, Show c) => [(a,b,c)] -> Range
tuple3InColumns = map (\(a,b,c) -> [textify a, textify b, textify c])

tuple4InColumns :: (Show a, Show b, Show c, Show d) => [(a,b,c,d)] -> Range
tuple4InColumns = map (\(a,b,c,d) -> [textify a, textify b, textify c, textify d])

tuple5InColumns :: (Show a, Show b, Show c, Show d, Show e) => [(a,b,c,d,e)] -> Range
tuple5InColumns = map (\(a,b,c,d,e) -> [textify a, textify b, textify c, textify d, textify e])

tuple6InColumns :: (Show a, Show b, Show c, Show d, Show e, Show f) => [(a,b,c,d,e,f)] -> Range
tuple6InColumns = map (\(a,b,c,d,e,f) -> [textify a, textify b, textify c, textify d, textify e, textify f])

timestampWithList :: Show a => [(Text,[a])] -> Range
timestampWithList = map (\(a,b) -> a : map textify b)

list2Columns :: Show a => [a] -> Range
list2Columns ls = [ map textify ls ]

transpose :: Range -> Range
transpose = getZipList . traverse ZipList

textify :: (Show a) => a -> Text
textify = pack . show

