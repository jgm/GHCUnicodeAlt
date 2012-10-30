{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import qualified GHC.Unicode           as U
import qualified GHC.UnicodeAlt        as Alt
import           Criterion             (bench, bgroup, nf, Benchmark)
import           Criterion.Main        (defaultMain)
import           Control.DeepSeq       (NFData)
import           Control.Monad
import qualified Data.Char             as C
----------------------------------------------------------------

sophocles :: String
sophocles = "Ἰοὺ ἰού· τὰ πάντʼ ἂν ἐξήκοι σαφῆ.\nὮ φῶς, τελευταῖόν σε προσϐλέψαιμι νῦν,\nὅστις πέφασμαι φύς τʼ ἀφʼ ὧν οὐ χρῆν, ξὺν οἷς τʼ\nοὐ χρῆν ὁμιλῶν, οὕς τέ μʼ οὐκ ἔδει κτανών.\nἸοὺ ἰού· τὰ πάντʼ ἂν ἐξήκοι σαφῆ.\nὮ φῶς, τελευταῖόν σε προσϐλέψαιμι νῦν,\nὅστις πέφασμαι φύς τʼ ἀφʼ ὧν οὐ χρῆν, ξὺν οἷς τʼ\nοὐ χρῆν ὁμιλῶν, οὕς τέ μʼ οὐκ ἔδει κτανών."

lorem :: String
lorem = "Lorem ipsum dolor sit amet, consectetur adipisicing elit,\nsed do eiusmod tempor incididunt ut labore et\ndolore magna aliqua. Ut enim ad minim veniam,\nquis nostrud exercitation ullamco laboris nisi ut\naliquip ex ea commodo consequat. Duis aute irure dolor\nin reprehenderit in voluptate velit esse cillum dolore\neu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,\nsunt in culpa qui officia deserunt mollit anim id est laborum.\n"

lorem_shortlines :: String
lorem_shortlines = unlines $ map (++"\n") $ words lorem

lorem_longlines :: String
lorem_longlines = unwords $ words $ lorem

haskell :: String
haskell = "isSpace_DataChar :: Char -> Bool\n{-# INLINE isSpace_DataChar #-}\n  isSpace_DataChar c =\n     c == ' '     ||\n     c == '\t'    ||\n     c == '\n'    ||\n     c == '\r'    ||\n     c == '\f'    ||\n\tc == '\v'    ||\n\tc == '\xa0'  ||\n\tiswspace (fromIntegral (C.ord c)) /= 0\nisSpace_DataChar :: Char -> Bool\n{-# INLINE isSpace_DataChar #-}\n  isSpace_DataChar c =\n     c == ' '     ||\n     c == '\t'    ||\n     c == '\n'    ||\n     c == '\r'    ||\n     c == '\f'    ||\n\tc == '\v'    ||\n\tc == '\xa0'  ||\n\tiswspace (fromIntegral (C.ord c)) /= 0\n"

checkCorrectness :: Eq a => String -> (Char -> a) -> (Char -> a) -> IO ()
checkCorrectness name f1 f2 = do
  putStrLn $ "Checking correctness of " ++ name
  forM_ ['\x0'..'\x12000'] $ \x ->
    unless (f1 x == f2 x) $ error "FAILED!"

main :: IO ()
main = do
  checkCorrectness "Alt.isSpace" C.isSpace Alt.isSpace
  checkCorrectness "Alt.isUpper" C.isUpper Alt.isUpper
  checkCorrectness "Alt.isLower" C.isLower Alt.isLower
  checkCorrectness "Alt.toUpper" C.toUpper Alt.toUpper
  checkCorrectness "Alt.toLower" C.toLower Alt.toLower
  checkCorrectness "Alt.toTitle" C.toTitle Alt.toTitle
  defaultMain $
      benchLong [("U.isSpace",U.isSpace)
                ,("C.isSpace",C.isSpace)
                ,("Alt.isSpace",Alt.isSpace)]
   ++ benchFuns [("U.toUpper",U.toUpper)
                ,("C.toUpper",C.toUpper)
                ,("Alt.toUpper",Alt.toUpper)]
   ++ benchFuns [("U.toLower",U.toLower)
                ,("C.toLower",C.toLower)
                ,("Alt.toLower",Alt.toLower)]
   ++ benchFuns [("U.toTitle",U.toTitle)
                ,("C.toTitle",C.toTitle)
                ,("Alt.toTitle",Alt.toTitle)]
   ++ benchFuns [("U.isUpper",U.isUpper)
                ,("C.isUpper",C.isUpper)
                ,("Alt.isUpper",Alt.isUpper)]
   ++ benchFuns [("U.isLower",U.isLower)
                ,("C.isLower",C.isLower)
                ,("Alt.isLower",Alt.isLower)]

benchFuns:: (Eq a, NFData a) => [(String, Char -> a)] -> [Benchmark]
benchFuns funs =
          [ group funs "ascii text" lorem
          , group funs "Greek text" sophocles
          , group funs "chars 0..255" ['\0'..'\255']
          ]

benchLong :: (Eq a, NFData a) => [(String, Char -> a)] -> [Benchmark]
benchLong funs =
          [ group funs "ascii text" lorem
          , group funs "ascii text (short lines)" lorem_shortlines
          , group funs "ascii text (long lines)" lorem_longlines
          , group funs "Greek text" sophocles
          , group funs "Haskell code" haskell
          , group funs "chars 0..255" ['\0'..'\255']
          , group funs "all spaces" $ concat $ replicate 50 "\t\r\n\n "
          ]

group :: NFData a => [(String, Char -> a)] -> String -> String -> Benchmark
group funs name inp = bgroup name $
        map (\(n, fun) -> bench n $ nf (map fun) inp) funs

