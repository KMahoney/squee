module RangedParsec.Pretty
  ( prettyRange
  , prettyPos
  ) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import RangedParsec.Pos


prettyRange :: SourceSpan -> Doc AnsiStyle
prettyRange sourceSpan =
   vcat (map hlLine numberedLines)

  where
    ((startLine, startColumn), (endLine, endColumn)) = discardSource sourceSpan

    content = sourceContent sourceSpan
    
    line0 = startLine - 1
    startCol0 = startColumn - 1
    endCol0 = endColumn - 1

    context = 2
    contextStart = max 0 (line0 - context)
    contextTopHeight = line0 - contextStart
    highlightHeight = (endLine - startLine) + 1
    contextHeight = contextTopHeight + highlightHeight + context

    numberedLines :: [(Int, T.Text)]
    numberedLines =
      take contextHeight $ drop contextStart $ zip [(1 :: Int) ..] $ T.splitOn "\n" content

    hlStyle = color Red <> underlined

    hlLine :: (Int, T.Text) -> Doc AnsiStyle
    hlLine (n, l)
      | n < startLine =
        lineNumber n <+> pretty l
      | n > endLine =
        lineNumber n <+> pretty l
      | n == startLine && n == endLine =
        lineNumber n <+>
        pretty (T.take startCol0 l) <>
        annotate hlStyle (pretty (T.take (endCol0 - startCol0) $ T.drop startCol0 l)) <>
        pretty (T.drop endCol0 l)
      | n == startLine =
        lineNumber n <+>
        pretty (T.take startCol0 l) <>
        annotate hlStyle (pretty (T.drop startCol0 l))
      | n == endLine =
        lineNumber n <+>
        annotate hlStyle (pretty (T.take endCol0 l)) <>
        pretty (T.drop endCol0 l)
      | otherwise =
        lineNumber n <+> annotate hlStyle (pretty l)

    numberPadding :: Int
    numberPadding =
      foldr (max . length . show . fst) 0 numberedLines

    paddedNumber :: Int -> Doc AnsiStyle
    paddedNumber n =
      pretty (replicate (numberPadding - length (show n)) ' ') <> annotate (color Blue) (pretty n)

    lineNumber :: Int -> Doc AnsiStyle
    lineNumber n = paddedNumber n <> ":"


prettyPos :: SourcePos -> Doc AnsiStyle
prettyPos sourcePos =
   vcat (map hlLine numberedLines)

  where
    (posLine, posColumn) = discardSource sourcePos
    
    content = sourceContent sourcePos

    line0 = posLine - 1

    context = 2
    contextStart = max 0 (line0 - context)
    contextTopHeight = line0 - contextStart
    contextHeight = contextTopHeight + 1 + context

    numberedLines :: [(Int, T.Text)]
    numberedLines =
      take contextHeight $ drop contextStart $ zip [(1 :: Int) ..] $ T.splitOn "\n" content

    hlStyle = color Red

    hlLine (n, l)
      = lineNumber n <+> pretty l <>
      if n == posLine
      then line <> fill (posColumn + numberPadding + 1) " " <> annotate hlStyle "^"
      else mempty

    numberPadding :: Int
    numberPadding =
      foldr (max . length . show . fst) 0 numberedLines

    paddedNumber :: Int -> Doc AnsiStyle
    paddedNumber n =
      pretty (replicate (numberPadding - length (show n)) ' ') <> annotate (color Blue) (pretty n)

    lineNumber :: Int -> Doc AnsiStyle
    lineNumber n = paddedNumber n <> ":"
