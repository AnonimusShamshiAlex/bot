module Bot.Pptx (renderDeck) where

import Control.Monad (forM)
import Data.Bits (complement, shiftR, testBit, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word32)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Bot.Types

data Entry = Entry
  { entryName :: FilePath
  , entryData :: BS.ByteString
  }

data Palette = Palette
  { paperColor :: Text
  , inkColor :: Text
  , accentColor :: Text
  , mutedColor :: Text
  }

palette :: Theme -> Palette
palette Light = Palette "FFFFFF" "1F2937" "2563EB" "6B7280"
palette Dark = Palette "111827" "F9FAFB" "38BDF8" "9CA3AF"
palette Blue = Palette "1E3A8A" "FFFFFF" "FBBF24" "BFDBFE"

num :: Int -> Text
num = T.pack . show

esc :: Text -> Text
esc = T.concatMap f . T.filter ok
  where
    ok c = c >= ' ' || c == '\t'
    f '&' = "&amp;"
    f '<' = "&lt;"
    f '>' = "&gt;"
    f '"' = "&quot;"
    f c = T.singleton c

xmlHead :: Text
xmlHead = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"

rect :: Int -> Text -> (Int, Int, Int, Int) -> Text
rect i color (x, y, w, h) = T.concat
  [ "<p:sp><p:nvSpPr><p:cNvPr id=\"", num i, "\" name=\"Accent ", num i, "\"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr>"
  , "<p:spPr><a:xfrm><a:off x=\"", num x, "\" y=\"", num y, "\"/><a:ext cx=\"", num w, "\" cy=\"", num h, "\"/></a:xfrm>"
  , "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:solidFill><a:srgbClr val=\"", color, "\"/></a:solidFill>"
  , "<a:ln><a:noFill/></a:ln></p:spPr></p:sp>"
  ]

holder :: Int -> Text -> Text -> (Int, Int, Int, Int) -> Text -> Text -> Text
holder i name ph (x, y, w, h) anchor paras = T.concat
  [ "<p:sp><p:nvSpPr><p:cNvPr id=\"", num i, "\" name=\"", name, "\"/>"
  , "<p:cNvSpPr><a:spLocks noGrp=\"1\"/></p:cNvSpPr><p:nvPr>", ph, "</p:nvPr></p:nvSpPr>"
  , "<p:spPr><a:xfrm><a:off x=\"", num x, "\" y=\"", num y, "\"/><a:ext cx=\"", num w, "\" cy=\"", num h, "\"/></a:xfrm></p:spPr>"
  , "<p:txBody><a:bodyPr anchor=\"", anchor, "\"><a:normAutofit/></a:bodyPr><a:lstStyle/>", paras, "</p:txBody></p:sp>"
  ]

run :: Text -> Int -> Bool -> Text -> Text
run color sz bold t = T.concat
  [ "<a:r><a:rPr lang=\"ru-RU\" sz=\"", num sz, "\"", if bold then " b=\"1\"" else "", " dirty=\"0\">"
  , "<a:solidFill><a:srgbClr val=\"", color, "\"/></a:solidFill></a:rPr><a:t>", esc t, "</a:t></a:r>"
  ]

plainPara :: Text -> Text
plainPara r = "<a:p><a:pPr marL=\"0\" indent=\"0\" algn=\"l\"><a:buNone/></a:pPr>" <> r <> "</a:p>"

bulletPara :: Palette -> Int -> Text -> Text
bulletPara p sz t = T.concat
  [ "<a:p><a:pPr marL=\"342900\" indent=\"-342900\" algn=\"l\"><a:spcBef><a:spcPts val=\"1000\"/></a:spcBef>"
  , "<a:buClr><a:srgbClr val=\"", accentColor p, "\"/></a:buClr><a:buFont typeface=\"Arial\"/><a:buChar char=\"•\"/></a:pPr>"
  , run (inkColor p) sz False t, "</a:p>"
  ]

slideXml :: Palette -> [Text] -> Text
slideXml p shapes = T.concat
  [ xmlHead
  , "<p:sld xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\""
  , " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\""
  , " xmlns:p=\"http://schemas.openxmlformats.org/presentationml/2006/main\">"
  , "<p:cSld><p:bg><p:bgPr><a:solidFill><a:srgbClr val=\"", paperColor p, "\"/></a:solidFill><a:effectLst/></p:bgPr></p:bg><p:spTree>"
  , "<p:nvGrpSpPr><p:cNvPr id=\"1\" name=\"\"/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr>"
  , "<p:grpSpPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/><a:chOff x=\"0\" y=\"0\"/><a:chExt cx=\"0\" cy=\"0\"/></a:xfrm></p:grpSpPr>"
  , T.concat shapes
  , "</p:spTree></p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>"
  ]

titleSlide :: Palette -> Text -> Text -> Text
titleSlide p title date = slideXml p
  [ rect 2 (accentColor p) (0, 0, 228600, 6858000)
  , holder 3 "Title 1" "<p:ph type=\"ctrTitle\"/>" (914400, 1371600, 10363200, 2057400) "b"
      (plainPara (run (inkColor p) (if T.length title <= 40 then 5400 else 4000) True title))
  , rect 4 (accentColor p) (914400, 3657600, 1828800, 64008)
  , holder 5 "Subtitle 2" "<p:ph type=\"subTitle\" idx=\"1\"/>" (914400, 3886200, 10363200, 640080) "t"
      (plainPara (run (mutedColor p) 2400 False date))
  ]

contentSlide :: Palette -> Slide -> Text
contentSlide p (Slide title ls)
  | null ls = slideXml p
      [ rect 2 (accentColor p) (0, 0, 228600, 6858000)
      , holder 3 "Title 1" "<p:ph type=\"title\"/>" (914400, 2286000, 10363200, 2286000) "ctr"
          (plainPara (run (inkColor p) 4800 True title))
      ]
  | otherwise = slideXml p
      [ rect 2 (accentColor p) (0, 0, 228600, 6858000)
      , holder 3 "Title 1" "<p:ph type=\"title\"/>" (685800, 365760, 10820400, 1051560) "ctr"
          (plainPara (run (inkColor p) (if T.length title <= 50 then 3600 else 2800) True title))
      , rect 4 (accentColor p) (685800, 1463040, 1371600, 45720)
      , holder 5 "Content 2" "<p:ph idx=\"1\"/>" (685800, 1737360, 10820400, 4389120) "t"
          (T.concat (map (bulletPara p (bodySize ls)) ls))
      ]

bodySize :: [Text] -> Int
bodySize ls
  | n <= 4 && m <= 70 = 2800
  | n <= 7 && m <= 100 = 2400
  | otherwise = 2000
  where
    n = length ls
    m = maximum (map T.length ls)

slideRels :: Int -> Text
slideRels layout = T.concat
  [ xmlHead
  , "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
  , "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout"
  , num layout, ".xml\"/></Relationships>"
  ]

listFiles :: FilePath -> IO [FilePath]
listFiles root = go ""
  where
    go rel = do
      names <- listDirectory (root </> rel)
      fmap concat . forM (sort names) $ \n -> do
        let r = if null rel then n else rel <> "/" <> n
        isDir <- doesDirectoryExist (root </> r)
        if isDir then go r else pure [r]

adjust :: Int -> Text -> Entry -> Entry
adjust n title e = case entryName e of
  "[Content_Types].xml" -> edit (T.replace "</Types>" (overrides <> "</Types>"))
  "ppt/_rels/presentation.xml.rels" -> edit (T.replace "</Relationships>" (relations <> "</Relationships>"))
  "ppt/presentation.xml" -> edit (T.replace "<p:sldSz" (idList <> "<p:sldSz"))
  "docProps/core.xml" -> edit (T.replace "<dc:title/>" ("<dc:title>" <> esc title <> "</dc:title>"))
  _ -> e
  where
    edit f = e { entryData = TE.encodeUtf8 (f (TE.decodeUtf8 (entryData e))) }
    ids = [1 .. n]
    overrides = T.concat
      [ "<Override PartName=\"/ppt/slides/slide" <> num i <> ".xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.slide+xml\"/>"
      | i <- ids
      ]
    relations = T.concat
      [ "<Relationship Id=\"rIdSlide" <> num i <> "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide\" Target=\"slides/slide" <> num i <> ".xml\"/>"
      | i <- ids
      ]
    idList = "<p:sldIdLst>" <> T.concat
      [ "<p:sldId id=\"" <> num (255 + i) <> "\" r:id=\"rIdSlide" <> num i <> "\"/>"
      | i <- ids
      ] <> "</p:sldIdLst>"

crc32 :: BS.ByteString -> Word32
crc32 = complement . BS.foldl' (\c b -> go (8 :: Int) (c `xor` fromIntegral b)) 0xFFFFFFFF
  where
    go 0 x = x
    go k x = go (k - 1) (if testBit x 0 then (x `shiftR` 1) `xor` 0xEDB88320 else x `shiftR` 1)

nameBytes :: Entry -> BS.ByteString
nameBytes = TE.encodeUtf8 . T.pack . entryName

localRecord :: Entry -> BS.ByteString
localRecord e = BL.toStrict . B.toLazyByteString $ mconcat
  [ B.word32LE 0x04034b50
  , B.word16LE 20
  , B.word16LE 0x0800
  , B.word16LE 0
  , B.word16LE 0
  , B.word16LE 0x21
  , B.word32LE (crc32 (entryData e))
  , B.word32LE size
  , B.word32LE size
  , B.word16LE (fromIntegral (BS.length (nameBytes e)))
  , B.word16LE 0
  , B.byteString (nameBytes e)
  , B.byteString (entryData e)
  ]
  where
    size = fromIntegral (BS.length (entryData e))

centralRecord :: Word32 -> Entry -> BS.ByteString
centralRecord offset e = BL.toStrict . B.toLazyByteString $ mconcat
  [ B.word32LE 0x02014b50
  , B.word16LE 20
  , B.word16LE 20
  , B.word16LE 0x0800
  , B.word16LE 0
  , B.word16LE 0
  , B.word16LE 0x21
  , B.word32LE (crc32 (entryData e))
  , B.word32LE size
  , B.word32LE size
  , B.word16LE (fromIntegral (BS.length (nameBytes e)))
  , B.word16LE 0
  , B.word16LE 0
  , B.word16LE 0
  , B.word16LE 0
  , B.word32LE 0
  , B.word32LE offset
  , B.byteString (nameBytes e)
  ]
  where
    size = fromIntegral (BS.length (entryData e))

zipStored :: [Entry] -> BS.ByteString
zipStored es = BS.concat (locals <> centrals <> [eocd])
  where
    locals = map localRecord es
    offsets = scanl (+) 0 (map (fromIntegral . BS.length) locals) :: [Word32]
    centrals = zipWith centralRecord offsets es
    cdSize = fromIntegral (sum (map BS.length centrals))
    cdOffset = fromIntegral (sum (map BS.length locals))
    count = fromIntegral (length es)
    eocd = BL.toStrict . B.toLazyByteString $ mconcat
      [ B.word32LE 0x06054b50
      , B.word16LE 0
      , B.word16LE 0
      , B.word16LE count
      , B.word16LE count
      , B.word32LE cdSize
      , B.word32LE cdOffset
      , B.word16LE 0
      ]

renderDeck :: FilePath -> Deck -> IO BS.ByteString
renderDeck root deck = do
  names <- listFiles root
  base <- forM names $ \n -> Entry n <$> BS.readFile (root </> n)
  now <- getCurrentTime
  let date = T.pack (formatTime defaultTimeLocale "%d.%m.%Y" now)
      pal = palette (deckTheme deck)
      bodies = titleSlide pal (deckTitle deck) date : map (contentSlide pal) (deckSlides deck)
      layouts = 1 : repeat 2
      count = length bodies
      extra = concat
        [ [ Entry ("ppt/slides/slide" <> show i <> ".xml") (TE.encodeUtf8 body)
          , Entry ("ppt/slides/_rels/slide" <> show i <> ".xml.rels") (TE.encodeUtf8 (slideRels layout))
          ]
        | (i, body, layout) <- zip3 [1 :: Int ..] bodies layouts
        ]
      patched = map (adjust count (deckTitle deck)) base
      (head', rest) = span ((== "[Content_Types].xml") . entryName) patched
  pure (zipStored (head' <> rest <> extra))
