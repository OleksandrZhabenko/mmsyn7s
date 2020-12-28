-- |
-- Module      :  MMSyn7s
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that show a sorted list of the Ukrainian sounds 
-- representations that can be used by mmsyn7 series of programs.
--

module MMSyn7s (
  -- * Used in the program
  main7s
  -- * Library functions
  -- ** For the text as a whole object
  , show7s2
  -- ** For the text being treated as partial one
  , show7s'
  , show7s''
  , show7s'''
  , show7s3
  , show7s4
  , show7s5
  , show7s6
  , show7s7
  , show7s8
  , show7s9
  -- *** Inner predicate (auxiliary)
  , eqSnds
  -- *** Inner backward conversion function
  , listToString
  -- * Some descriptive statistics metrices
  , countSnds
  , countSnds2
  , sndsDensity
  , uniquenessPeriods
  , uniqMax
  , uniqMin
  , uniqPeriodsMean
  , uniqPeriodsDispersion
  , uniqStdQDeviation
) where

import Data.Char (isSpace)
import qualified Data.Vector as V
import Data.List (sort, nub,(\\),nubBy)
import Melodics.Ukrainian (convertToProperUkrainian)
import System.Environment (getArgs)
import MMSyn7.Syllable
import Control.Exception (onException)
import MMSyn6Ukr.Show7s

-- | Function takes the first command line argument and (may be) a Ukrainian text being written without quotes as the next command line arguments
-- and prints the sorted list of the Ukrainian sounds representations that can be used further in mmsyn7 series of programs.
--
-- Depending on the first command line argument the program behaves as follows:
-- 
-- \"-h\" -- prints help and exits;
-- 
-- \"-v\" -- prints version number and exits;
--
-- \"-s\" -- prints some general descriptive statistics metrices for the given text; 
--
-- \"-s2\" -- prints a syllable segmentation for the {number of words (or their parts) to be taken for statistics, which is a second command line argument} and 
-- some information about its structure that can be interesting.
-- 
-- \"1\"  -- prints the list of String being unique (without silence) and then the rest of the text with whitespaces and some phonetical conversions;
-- 
-- \"-1\" -- prints the rest of the text after the first duplicated sound representation (except silent ones) including it with whitespaces and some phonetical conversions;
-- 
-- \"0\"  -- prints the list of String being the Ukrainian sounds representations for the whole text.
--
-- \"2\"  -- prints the list of String being the Ukrainian sounds representations for the whole text where every sound representation is unique; 
--
-- \"3\"  -- prints the list of lists of Strings being the Ukrainian sounds representations for the whole text, which shows what sound representations
-- are needed to be created if every sound is unique;
-- 
-- All other variants of the beginning for the command line arguments are the same as \"0\" (the arguments are treated as a Ukrainian text
-- and processed as a whole one object).
main7s :: IO ()
main7s = do 
  texts <- getArgs
  let arg1 = concat . take 1 $ texts in
    case arg1 of
      "-h" -> do { putStrLn "mmsyn7s: "
                 ; putStrLn "SYNOPSYS: "
                 ; putStrLn "mmsyn7s -h      OR: "
                 ; putStrLn "mmsyn7s -v      OR: "
                 ; putStrLn "mmsyn7s -s {Ukrainian text}     OR: "
                 ; putStrLn "mmsyn7s -s2 {number of words (or their parts) to be taken for statistics} {Ukrainian text}     OR: "
                 ; putStrLn "mmsyn7s 1 {Ukrainian text}     OR: "
                 ; putStrLn "mmsyn7s -1 {Ukrainian text}     OR: "
                 ; putStrLn "mmsyn7s 0 {Ukrainian text}     OR: "
                 ; putStrLn "mmsyn7s {Ukrainian text}"
                 ; putStrLn "where the first one prints this help message; "
                 ; putStrLn "      the second one prints a version number; "
                 ; putStrLn "      the third one prints some general descriptive statistics metrices for the given text; "
                 ; putStr "      the fourth one prints a syllable segmentation for the {number of words (or their parts) to be taken for statistics} and "
                 ; putStrLn "some information about its structure that can be interesting."
                 ; putStrLn "      the \"1\" option prints the list of String being the Ukrainian sounds representations and being unique alongside the text (without silence);"
                 ; putStr   "      the \"-1\" option prints the rest of the text after the first duplicated sound representation (except silent ones); "
                 ; putStrLn "including it with whitespaces and some phonetical conversions; "
                 ; putStrLn "      the \"0\" option prints the list of String  being the Ukrainian sounds representations for the whole text; "
                 ; putStrLn "      the \"2\" option prints the list of String  being the Ukrainian sounds representations for the whole text where every sound representation is unique; "
                 ; putStr "      the \"3\" option prints the list of lists of Strings  being the Ukrainian sounds representations for the whole text, "
                 ; putStrLn "which shows what sound representations are needed to be created if every sound is unique; "
                 ; putStrLn "      the other beginning is equivalent to the \"0\" behaviour."
                  }
      "-v" -> putStrLn "mmsyn7s: version 0.8.0.0"
      "-s" -> let ys = unwords . drop 1 $ texts
                  zs = uniquenessPeriods ys in
                  if zs /= [0::Int]
                    then do { let t  = sum zs
                                  m  = length zs
                                  n  = fromIntegral t / fromIntegral m
                                  d  = ((sum . fmap (\w -> (fromIntegral w - n) * (fromIntegral w - n)) $ zs) / fromIntegral m)
                                  s  = sqrt d
                                  mx = maximum zs
                                  mn = minimum zs
                            ; putStrLn "---------------------------------------------------------------------------"
                            ; putStrLn $ "Uniqueness periods:                                                      " ++ (show zs)
                            ; putStrLn $ "Possibly unique sounds representations density:                          " ++ show (fromIntegral t / (fromIntegral . length . show7s $ ys))
                            ; putStrLn $ "Number of sounds representations that are enough to cover the text:      " ++ show t
                            ; putStrLn $ "Mean for the uniqueness periods list:                                    " ++ show n
                            ; putStrLn $ "Dispersion for the uniqueness periods list:                              " ++ show d
                            ; putStrLn $ "Standard quadratic deviation for the uniqueness periods list:            " ++ show s
                            ; putStrLn $ "The maximum element in the uniqueness periods list:                      " ++ show mx
                            ; putStrLn $ "The minimum element in the uniqueness periods list:                      " ++ show mn }
                    else do { putStrLn "------------------------------------------------------------------------------"
                            ; putStrLn $ "Uniqueness periods:                                                      [0]"
                            ; putStrLn $ "Possibly unique sounds representations density:                          0.0"
                            ; putStrLn $ "Number of sounds representations that are enough to cover the text:      0"
                            ; putStrLn $ "Mean for the uniqueness periods list:                                    0.0"
                            ; putStrLn $ "Dispersion for the uniqueness periods list:                              0.0"
                            ; putStrLn $ "Standard quadratic deviation for the uniqueness periods list:            0.0"
                            ; putStrLn $ "The maximum element in the uniqueness periods list:                      0"
                            ; putStrLn $ "The minimum element in the uniqueness periods list:                      0" }
      "-s2" -> let n = concat . drop 1 . take 2 $ texts in exceptRead n (drop 2 texts)
      "1"  -> print . fst . show7s5 . unwords . drop 1 $ texts
      "-1" -> putStrLn . snd . show7s5 . unwords . drop 1 $ texts
      "0"  -> putStrLn . show7s2 . unwords . drop 1 $ texts
      "2"  -> print . show7s8 . unwords . drop 1 $ texts
      "3"  -> print . show7s6 . unwords . drop 1 $ texts
      _    -> putStrLn . show7s2 . unwords $ texts

-- | Is used internally in the 'exceptRead' function.
printStatSyl :: String -> [String] -> IO ()
printStatSyl xs yss =
  do { let m = read xs :: Int
           zss = takeWordSP m . unwords $ yss
           tss = sylLengthsP2 zss     
     ; putStrLn "---------------------------------------------------------------------------"
     ; putStrLn $ "Syllables in the text:                                                   " ++ (show . fmap (fmap (concatMap show)) $ zss)
     ; putStrLn "---------------------------------------------------------------------------"
     ; putStrLn $ "Number of the Ukrainian sounds in the syllables in the text:             " ++ show tss
     ; putStrLn ""
     }

-- | Is used internally in the 'mmsyn7s' function.
exceptRead :: String -> [String] -> IO ()
exceptRead xs yss = onException (printStatSyl xs yss)
  (do { putStrLn "Please, specify a number of words (or their parts being created with apostrophe or dash (hyphen) signs (or both)) "
      ; ns <- getLine
      ; exceptRead ns yss
      })
  
-- | Function takes a Ukrainian text being a @String@ and returns a @String@ that shows a sorted list of the Ukrainian sounds representations that can be used further
-- in mmsyn7 series of programs.
show7s2 :: String -> String
show7s2 = show . sort . nub . V.toList . V.filter (\x -> x /= "-" && x /= "1" && x /= "0") . convertToProperUkrainian

-- | Function 'show7s3' takes a Ukrainian text being a @String@ and returns a tuple, the first element of which is a list of Strings that correspond to the Ukrainian 
-- sounds representations that (except pauses) are unique and are not repeated starting from the beginning of the given text, and the second one is a remainder
-- list of Strings starting from the first duplicated non-silent Ukrainian sound representation.
show7s3 :: String -> ([String], [String])
show7s3 = show7s' . V.toList . convertToProperUkrainian

-- | Function 'eqSnds' compares two non-silent Strings representations for Ukrainian sounds by equality. If one of them is a representation for silence (e. g. pause),
-- then the predicate is @False@.
eqSnds :: String -> String -> Bool
eqSnds xs ys | xs `elem` ["-","0","1"] || ys `elem` ["-","0","1"] = False
             | otherwise = xs == ys

-- | Function @show7s'@ is auxiliary to the 'show7s3' and is used internally in the latter one.
show7s' :: [String] -> ([String],[String])
show7s' zss =
  let (xss, yss) = splitAt 68 zss
      uss = xss \\ nubBy eqSnds xss
      (wss, vss) = if null uss then (xss,[]) else (takeWhile (/= head uss) xss ++ head uss:(takeWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss),
        dropWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss) in
          (wss, vss ++ yss)

-- | The same as @show7s'@, but the first list in the tuple is filtered from the silent representations and is sorted not in the order of appearance in the text,
-- but in the ascending order.
show7s'' :: [String] -> ([String],[String])
show7s'' zss =
  let (xss, yss) = splitAt 68 zss
      uss = xss \\ nubBy eqSnds xss
      (wss,vss) = if null uss then (xss,[]) else (takeWhile (/= head uss) xss ++ head uss:(takeWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss),
        dropWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss) in 
          (sort . filter (\x -> x /= "-" && x /= "1" && x /= "0") $ wss,vss ++ yss)

-- | Function 'show7s4' takes a Ukrainian text being a @String@ and returns a tuple, the first element of which is a list of Strings that correspond to the Ukrainian 
-- sounds representations that (except pauses) are unique and are not repeated starting from the beginning of the given text (this list is filtered from 
-- the representations for the silence and then sorted in the ascending order), and the second one is a remainder
-- list of Strings starting from the first duplicated non-silent Ukrainian sound representation.
show7s4 :: String -> ([String], [String])
show7s4 = show7s'' . V.toList . convertToProperUkrainian

-- | Function 'listToString' converts the list of Strings being the sequential Ukrainian sounds representations into the Ukrainian text with whitespaces
-- (whitespaces are substituted instead of punctuation symbols, too) and some phonetical conversions.
listToString :: [String] -> String
listToString =
  concatMap (\t ->
    case t of
      "0" -> " "
      "1" -> " "
      "-" -> " "
      x   -> x)

-- | The same as @show7s''@, but the second element in the resulting tuple is again the Ukrainian text with whitespaces (whitespaces are substituted
-- instead of punctuation symbols, too) and some phonetical conversions.
show7s''' :: [String] -> ([String],String)
show7s''' zss =
  let (xss, yss) = splitAt 68 zss
      uss = xss \\ nubBy eqSnds xss
      (wss,vss) = if null uss then (xss,[]) else (takeWhile (/= head uss) xss ++ head uss:(takeWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss),
        dropWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss) in 
          (sort . filter (\x -> x /= "-" && x /= "1" && x /= "0") $ wss, listToString $ vss ++ yss)

-- | Function 'show7s5' takes a Ukrainian text being a @String@ and returns a tuple, the first element of which is a list of Strings that correspond to the Ukrainian 
-- sounds representations that (except pauses) are unique and are not repeated starting from the beginning of the given text (this list is filtered from 
-- the representations for the silence and then sorted in the ascending order), and the second one is a @String@ obtained from the remainder
-- list of Strings starting from the first duplicated non-silent Ukrainian sound representation with whitespaces (whitespaces are substituted
-- instead of punctiuation symbols, too) and some phonetical conversions. 
show7s5 :: String -> ([String], String)
show7s5 = show7s''' . V.toList . convertToProperUkrainian

-- | Function 'show7s6' takes a Ukrainian text being a @String@ and returns a list of lists of Strings, each latter one of which is obtained for the unique parts of
-- the text from the Ukrainian sounds representations point of view. It can show how many and what sound representations are needed to be created to completely cover
-- the given text providing all the needed sound parameters.
show7s6 :: String -> [[String]]
show7s6 t@(_:_) = (fst . show7s5 $ t):(show7s6 . snd . show7s5 $ t)
show7s6 _ = []

-- | Function 'countSnds' counts total number of Strings in the list of list of Strings. It can be successfully used to count how many Ukrainian sounds representations
-- are needed to be created to completely cover the given Ukrainian text. It can be used as a some statistics parameter for the text.
countSnds :: [[String]] -> Int
countSnds = sum . map length

-- | Function 'countSnds2' gives the same result as (countSnds . show7s6), but may be is sligtly more efficient in calculation.
countSnds2 :: String -> Int
countSnds2 = length . filter (\x -> x /= "-" && x /= "1" && x /= "0") . V.toList . convertToProperUkrainian

-- | Function 'sndsDensity' counts the ratio of total number of Ukrainian sounds representations (each of which gives an opportunity to use unique ones) to the
-- total number of the Ukrainian sounds representations if all the non-silent sounds in the text are the same for the one sound representation no matter where it is
-- located. It can be used as a some statistics parameter for the text. The greater is the result, the greater number of the Ukrainian sounds representations
-- is needed to be created for the text to create a unique sound for every location alongside the text. If it is equal to 1.0, then every non-silent sound
-- in the text appears just once (if at all appears).
sndsDensity :: String -> Double
sndsDensity xs | not . any (not . isSpace) $ xs = 1.0
               | otherwise =
  let x = countSnds . show7s6 $ xs
      y = length . show7s $ xs in fromIntegral x / fromIntegral y

-- | Function 'show7s7' takes a Ukrainian text being a @String@ and returns a tuple of Strings. The first element is a String corresponding to the beginning of the
-- text with only unique non-silent sounds representations, and the second one is the rest of the text. Each resulting String is modified so that it contains some
-- phonetical conversions and (may be) changed punctuation and whitespaces.
show7s7 :: String -> (String, String)
show7s7 xs = (listToString . fst $ y, listToString . snd $ y)
  where y = show7s3 xs

-- | Function 'show7s8' takes a Ukrainian text being a @String@ and returns a list of Strings. Each String contains only unique Ukrainian sounds representations so
-- that being sequenced from head of the list they all correspond to the whole text.
show7s8 :: String -> [String]
show7s8 t@(_:_) = (fst . show7s7 $ t):(show7s8 . snd . show7s7 $ t)
show7s8 _ = []

-- | Function 'show7s9' takes a Ukrainian text being a @String@ and returns a list of Strings. Each String is a Ukrainian sound representation of the duplicated 
-- non-silent sounds, it begins a new second list of Strings in the 'show7s4' function. This information can be helpful e. g. in music and composing.
show7s9 :: String -> [String]
show7s9 xs@(_:_) = (if not . null . snd . show7s4 $ xs then head . snd . show7s4 $ xs else []):(show7s9 . snd . show7s5 $ xs)
show7s9 _ = []

-- | Function 'uniquenessPeriods' takes a Ukrainian text being a @String@ and returns a list of Ints. Each Int value is a number of 
-- the Ukrainian sounds representations (non-silent ones) being unique and not duplicated alongside the given text starting from the beginning to the end.
-- This function provides some important information about the phonetical and in some cases semantical structures of the text.
uniquenessPeriods :: String -> [Int]
uniquenessPeriods xs | any (not . isSpace) xs = fmap length . show7s6 $ xs
                     | otherwise = [0::Int]

uniquenessPeriodsV :: String -> V.Vector Int
uniquenessPeriodsV xs 
 | any (not . isSpace) xs = V.fromList . fmap length . show7s6 $ xs
 | otherwise = V.singleton 0

-- | Function 'uniqPeriodsMean' is a mathematical expectation for the list obtained by 'uniquenessPeriods' function. It is a statistic metric.
-- It is a mean for the quantities of the unique (not duplicated, not repeated) Ukrainian sounds in the given Ukrainian text as a @String@.
-- If there are no Ukrainian letters in the text, it is equal to 0.0. The greater it is, the more diverse (phonetically) the text is.
uniqPeriodsMean :: String -> Float
uniqPeriodsMean xs | any (not . isSpace) xs = let ys = uniquenessPeriods xs in (fromIntegral . sum $ ys) / (fromIntegral . length $ ys)
                   | otherwise = 0.0

-- | Function 'uniqPeriodsDispersion' is a dispersion for the list obtained by 'uniquenessPeriods' function. It is a statistic metric.
-- It is a dispersion for the quantities of the unique (not duplicated, not repeated) Ukrainian sounds in the given Ukrainian text as a @String@.
-- If there are no Ukrainian letters in the text, it is equal to 0.0. The greater it is, the more suitable for changing pronunciation for the sounds 
-- (and may be for intonation changes, too) the text is.
uniqPeriodsDispersion :: String -> Float
uniqPeriodsDispersion xs | any (not . isSpace) xs =
  let ys = uniquenessPeriods xs
      z  = uniqPeriodsMean xs
      l  = length ys in
        (sum . map (\t -> (fromIntegral t - z) * (fromIntegral t - z)) $ ys) / fromIntegral l
                         | otherwise = 0.0

-- | Function 'uniqStdQDeviation' is a standard quadratic deviation for the list obtained by 'uniquenessPeriods' function. It is a statistic metric.
-- It is a standard quadratic deviation for the quantities of the unique (not duplicated, not repeated) Ukrainian sounds in the given Ukrainian text as a @String@.
-- If there are no Ukrainian letters in the text, it is equal to 0.0. The greater it is, the more suitable for changing pronunciation for the sounds
-- (and may be for intonation changes, too) the text is.
uniqStdQDeviation :: String -> Float
uniqStdQDeviation = sqrt . uniqPeriodsDispersion

-- | Function 'uniqMax' is a maximum element in the 'uniquenessPeriods' function for the same argument. It is provided as a standard element for the
-- descriptive statistics.
uniqMax :: String -> Int
uniqMax xs | any (not . isSpace) xs = maximum . uniquenessPeriods $ xs
           | otherwise = 0::Int

-- | Function 'uniqMin' is a minimum element in the 'uniquenessPeriods' function for the same argument. It is provided as a standard element for the
-- descriptive statistics.
uniqMin :: String -> Int
uniqMin xs | any (not . isSpace) xs = minimum . uniquenessPeriods $ xs
           | otherwise = 0::Int
