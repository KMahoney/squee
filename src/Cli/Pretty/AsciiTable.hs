module Cli.Pretty.AsciiTable
  ( Table(..)
  , Column(..)
  , Alignment(..)
  , render
  ) where

import Data.List (transpose)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

data Table
  = Table { columns :: [Column]
          , rows :: [[T.Text]]
          }

data Column
  = Column { header :: T.Text
           , alignment :: Alignment
           }

data Alignment
  = AlignLeft | AlignRight


columnWidth :: Column -> [T.Text] -> Int
columnWidth (Column { header }) rows =
  foldr max 0 (T.length header : map T.length rows)


render :: Table -> Doc a
render (Table { columns, rows }) =
  vcat $
  [ row (map (\c -> (alignment c, header c)) columns)
  , hcat (map (\w -> "+-" <> pretty (T.pack (replicate (w + 1) '-'))) widths) <> "+"
  ]
  ++ map (row . zip alignments) rows
  where
    widths :: [Int]
    widths = map (uncurry columnWidth) (zip columns (transpose rows))

    alignments :: [Alignment]
    alignments = map alignment columns

    pad :: Int -> Alignment -> T.Text -> T.Text
    pad w AlignLeft t = t <> T.pack (replicate (w - (T.length t)) ' ')
    pad w AlignRight t = T.pack (replicate (w - (T.length t)) ' ') <> t

    row :: [(Alignment, T.Text)] -> Doc a
    row cols = hcat (map (\(w, (colAlign, val)) -> "| " <> pretty (pad w colAlign val) <> " ") (zip widths cols)) <> "|"
