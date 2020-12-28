-- |
-- Module      :  MMSyn7.Syllable
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that show a sorted list of the Ukrainian sounds
-- representations that can be used by mmsyn7 series of programs. This module
-- works with syllable segmentation.
--

{-# LANGUAGE DeriveDataTypeable #-}

module MMSyn7.Syllable (
  -- * Datatype for the Ukrainian sounds representations
  UZP(..)
  -- * The resulting functions
  , syllablesUkr
  , takeWordS
  , sylLengths
  -- ** Working more with UZP format
  , syllablesUkrP
  , takeWordSP
  , sylLengthsP
  , sylLengthsP2
  , syllableDurations
  -- * Used internally
  , createSyllables
  , divideConsonants
  , groupConsonants
  , sndGroups
  , takeWithV
  , vecToUZPs
  , vecWords
  , uzpsToList
  , representProlonged
  , filterC
  , additionalF
  -- ** Dealing with sound representation durations
  , str2Durat1
  -- ** Working more with UZP format
  , createSyllablesP
  , takeWithVP
  -- * Auxiliary predicate functions
  , isNotVowel2
  , isSonorous1
  , isVoicedC1
  , isVoicelessC1
  , isVowel1
  , isVwl
  , notEqC
) where

import Data.Typeable
import qualified Data.Vector as V
import qualified Data.List as L (groupBy,span)
import Melodics.Ukrainian (convertToProperUkrainian)
import CaseBi (getBFst')
import Data.List.InnToOut.Basic (mapI)

-- Inspired by: https://github.com/OleksandrZhabenko/mm1/releases/tag/0.2.0.0

-- CAUTION: Please, do not mix with the show7s functions, they are not interoperable.

-- | Datatype 'UZP' is a simple sound classification type. Its peculiarity is that it is shown as a usual String (by design corresponding to the Ukrainian sound).
data UZP = Vowel String | Sonorous String | SonorousP String | Voiced String | VoicedP String | Voiceless String | VoicelessP String | VoicelessP2 String
  deriving ( Eq, Typeable )

instance Ord UZP
  where compare x y = compare (show x) (show y)

instance Show UZP
  where show (SonorousP xs) = xs ++ "ь"
        show (VoicedP xs) = xs ++ "ь"
        show (VoicelessP xs) = xs ++ "ь"
        show (Vowel xs) = xs
        show (VoicelessP2 xs) = xs
        show (Sonorous xs) = xs
        show (Voiced xs) = xs
        show (Voiceless xs) = xs

-- | Function 'vecToUZPs' is used to convert a 'V.Vector' of 'String' representing Ukrainian sounds to a list of 'UZP'.
vecToUZPs :: V.Vector String -> [UZP]
vecToUZPs v
  | V.null v = []
  | getBFst' (False, (V.fromList . zip ["а","е","и","о","у","і"] $ (replicate 6 True))) . V.unsafeHead $ v = Vowel (V.unsafeHead v):vecToUZPs (V.unsafeTail v)
  | V.unsafeHead v == "сь" || V.unsafeHead v == "ць" = VoicelessP2 (V.unsafeHead v):(vecToUZPs (V.unsafeTail v))
  | ((V.null . V.unsafeTail $ v) || (V.unsafeIndex v 1 /= "ь")) && getBFst' (False, V.fromList . zip ["в","й","л","м","н","р"] $ (replicate 6 True)) (V.unsafeHead v) = Sonorous (V.unsafeHead v):(if V.null . V.unsafeTail $ v then [] else vecToUZPs (V.unsafeTail v))
  | ((V.null . V.unsafeTail $ v) || (V.unsafeIndex v 1 /= "ь")) && getBFst' (False, V.fromList . zip ["б","г","д","дж","дз","ж","з","ґ"] $ (replicate 8 True)) (V.unsafeHead v) = Voiced (V.unsafeHead v):(if V.null . V.unsafeTail $ v then [] else vecToUZPs (V.unsafeTail v))
  | ((V.null . V.unsafeTail $ v) || (V.unsafeIndex v 1 /= "ь")) = Voiceless (V.unsafeHead v):(if V.null . V.unsafeTail $ v then [] else vecToUZPs (V.unsafeTail v))
  | getBFst' (False, V.fromList . zip ["в","л","м","н","р"] $ (replicate 5 True)) (V.unsafeHead v) = SonorousP (V.unsafeHead v):vecToUZPs (V.unsafeDrop 2 v)
  | getBFst' (False, V.fromList . zip ["б","г","д","дж","дз","ж","з","ґ"] $ (replicate 8 True)) (V.unsafeHead v) = VoicedP (V.unsafeHead v):vecToUZPs (V.unsafeDrop 2 v)
  | otherwise = VoicelessP (V.unsafeHead v):vecToUZPs (V.unsafeDrop 2 v)

-- | Function 'sndGroups' converts a Ukrainian word being a list of 'UZP' to the list of phonetically similar (consonants grouped with consonants and each vowel separately)
-- sounds representations in 'UZP' format.
sndGroups :: [UZP] -> [[UZP]]
sndGroups ys@(_:_) = L.groupBy isNotVowel2 ys
sndGroups _ = []

-- | Function-predicate 'isVowel1' checks whether its argument is a vowel representation in the 'UZP' format.
isVowel1 :: UZP -> Bool
isVowel1 (Vowel _) = True
isVowel1 _ = False

-- | Function-predicate 'isVwl' checks whether its argument is a vowel representation in the 'Char' format.
isVwl :: Char -> Bool
isVwl = getBFst' (False, (V.fromList . zip "аеиоуі" $ (replicate 6 True)))

-- | Function-predicate 'isSonorous1' checks whether its argument is a sonorous consonant representation in the 'UZP' format.
isSonorous1 :: UZP -> Bool
isSonorous1 (Sonorous _) = True
isSonorous1 (SonorousP _) = True
isSonorous1 _ = False

-- | Function-predicate 'isVoicedC1' checks whether its argument is a voiced consonant representation in the 'UZP' format.
isVoicedC1 ::  UZP -> Bool
isVoicedC1 (Voiced _) = True
isVoicedC1 (VoicedP _) = True
isVoicedC1 _ = False

-- | Function-predicate 'isVoiceless1' checks whether its argument is a voiceless consonant representation in the 'UZP' format.
isVoicelessC1 ::  UZP -> Bool
isVoicelessC1 (Voiceless _) = True
isVoicelessC1 (VoicelessP _) = True
isVoicelessC1 _ = False

-- | Binary function-predicate 'isNotVowel2' checks whether its arguments are both consonant representations in the 'UZP' format.
isNotVowel2 :: UZP -> UZP -> Bool
isNotVowel2 x y | isVowel1 x || isVowel1 y = False
                | otherwise = True

-- | Binary function-predicate 'notEqC' checks whether its arguments are not the same consonant sound representations (not taking palatalization into account).
notEqC :: UZP -> UZP -> Bool
notEqC x y =
  case x of
    (Vowel _) -> True
    _         ->
      case y of
        (Vowel _) -> True
        _         -> filterC (/= 'ь') x /= filterC (/= 'ь') y

-- | Auxiliary function used internally in the 'notEqC' function.
filterC :: (Char -> Bool) -> UZP -> String
filterC p t | isVowel1 t = show t
            | otherwise = filter p . show $ t

-- | Function 'vecWords' similarly to 'Prelude.words' divides a 'V.Vector' of 'String' into list of them, each element of which is a Ukrainian word (or its part
-- for dashed and hyphenated words or that ones with an apostrophe).
vecWords :: V.Vector String -> [V.Vector String]
vecWords v | V.null v = []
           | V.unsafeHead v == "-" || V.unsafeHead v == "0" || V.unsafeHead v == "1" = vecWords (V.unsafeTail v)
           | otherwise =
  let (v1, v2) = V.break (\x -> x == "-" || x == "0" || x == "1") v
      v3       = snd . V.span (\x -> x == "-" || x == "0" || x == "1") $ v2 in v1:vecWords v3

-- | Function 'divideConsonants' is used to divide groups of Ukrainian consonants into two-elements lists that later are made belonging to
-- different neighbour syllables if the group is between two vowels in a word. The group must be not empty, but this is not checked.
-- The phonetical information for the proper performance is taken from the:
-- https://msn.khnu.km.ua/pluginfile.php/302375/mod_resource/content/1/%D0%9B.3.%D0%86%D0%86.%20%D0%A1%D0%BA%D0%BB%D0%B0%D0%B4.%D0%9D%D0%B0%D0%B3%D0%BE%D0%BB%D0%BE%D1%81.pdf
divideConsonants :: [UZP] -> [[UZP]]
divideConsonants xs = case length xs of
  1 -> [[],xs]
  2 -> if ((isSonorous1 . head $ xs) && (head xs `notEqC` last xs)) || ((isVoicedC1 . head $ xs) && (isVoicelessC1 . head . tail $ xs)) then [[head xs], tail xs] else [[],xs]
  3 | isSonorous1 . head $ xs -> [[head xs], tail xs]
    | isSonorous1 . head . tail $ xs ->
      [[head xs, head . tail $ xs], [last xs]]
    | otherwise -> [[], xs]
  _ -> if (isSonorous1 . head $ xs) || (isVoicedC1 . head $ xs) then [[head xs], tail xs] else [[],xs]

-- | Function 'groupConsonants' is used to apply 'divideConsonants' to the needed groups of consonants.
groupConsonants :: [[UZP]] -> [[UZP]]
groupConsonants = mapI (not . isVowel1 . head) divideConsonants

-- | Function 'uzpsToList' converts a Ukrainian word being a list of syllables in 'UZP' format to a list of 'String'.
uzpsToList :: [[UZP]] -> [String]
uzpsToList = map (concatMap show)

-- | Function 'createSyllables' takes a prepared Ukrainian word and joins the parts (each one being a list of 'UZP') so that they constitute syllables in the 'String' format.
createSyllables :: [[UZP]] -> [String]
createSyllables xss =
  let (tss, vss) = L.span (any isVwl) . takeWithV $ xss in
    if null tss
      then [concat . takeWithV $ xss]
      else init tss ++ [last tss ++ concat vss]

-- | Function 'syllablesUkr' actually converts a 'String' to the list of words being segmented into the syllables in the 'String' format. If the Ukrainian word
-- being written down contains an apostrophe or a dash (hyphen) signs (or even both) then they are treated as separators for the distinguished words.
-- This does not influence the syllable structure and so the poetic characteristics of the text.
syllablesUkr :: String -> [[String]]
syllablesUkr = map ( createSyllables . additionalF) . vecWords . convertToProperUkrainian

-- Used internally in the 'syllablesUkr' and 'syllablesUkrP' functions.
additionalF :: V.Vector String -> [[UZP]]
additionalF = groupConsonants . sndGroups . vecToUZPs

-- | Function 'syllablesUkrP' actually converts a 'String' to the list of words being segmented into the syllables in the 'UZP' format. If the Ukrainian word being written down contains
-- an apostrophe or a dash (hyphen) signs (or even both) then they are treated as separators for the distinguished words. This does not influence the syllable structure
-- and so the poetic characteristics of the text.
syllablesUkrP :: String -> [[[UZP]]]
syllablesUkrP = map ( createSyllablesP . additionalF) . vecWords . convertToProperUkrainian

-- | Function 'takeWithV' is used internally in the 'createSyllables'.
takeWithV :: [[UZP]] -> [String]
takeWithV (x@(t:_):ys:xss)
  | isVowel1 t && null ys = show t:takeWithV xss
  | isVowel1 t && (isVowel1 . head $ ys) = show t:takeWithV (ys:xss)
  | isVowel1 t = (show t ++ (show . head $ ys)):takeWithV xss
  | otherwise = (concatMap show x ++ (head . takeWithV $ (ys:xss))):(tail . takeWithV $ (ys:xss))
takeWithV (_:ys:xss) = takeWithV (ys:xss)
takeWithV (x:_) = map show x
takeWithV _ = []

-- | Function 'takeWithVP' is used internally in the 'createSyllablesP'.
takeWithVP :: [[UZP]] -> [[UZP]]
takeWithVP (x@(t:_):ys:xss)
  | (isVowel1 t) && (null ys) = x:takeWithVP xss
  | (isVowel1 t) && (isVowel1 . head $ ys) = x:takeWithVP (ys:xss)
  | (isVowel1 t) = (x ++ ys):takeWithVP xss
  | otherwise = (x ++ (head . takeWithVP $ (ys:xss))):(tail . takeWithVP $ (ys:xss))
takeWithVP (_:ys:xss) = takeWithVP (ys:xss)
takeWithVP (x:_) = [x]
takeWithVP _ = []

-- | Function 'createSyllablesP' takes a prepared Ukrainian word and joins the parts (each one being a list of 'UZP') so that they constitute syllables in the 'UZP' format.
createSyllablesP :: [[UZP]] -> [[UZP]]
createSyllablesP xss =
  let (tss, vss) = L.span (any isVowel1) . takeWithVP $ xss in
    if null tss
      then [concat . takeWithVP $ xss]
      else init tss ++ [last tss ++ concat vss]

-- | Function 'takeWordS' takes a number (which is its first argument) of the Ukrainian words and represents them as a list of list of 'String', each of which is a syllable
-- in the 'String' format.
-- If the Ukrainian word being written down contains an apostrophe or a dash (hyphen) signs (or even both) then they are treated as separators for the distinguished words.
-- This does not influence the syllable structure and so the poetic characteristics of the text.
takeWordS :: Int -> String -> [[String]]
takeWordS n = take n . syllablesUkr

-- | Function 'takeWordSP' takes a number (which is its first argument) of the Ukrainian words and represents them as a list of list of list of 'UZP', each list of 'UZP' is a syllable.
-- If the Ukrainian word being written down contains an apostrophe or a dash (hyphen) signs (or even both) then they are treated as separators for the distinguished words.
-- This does not influence the syllable structure and so the poetic characteristics of the text.
takeWordSP :: Int -> String -> [[[UZP]]]
takeWordSP n = take n . syllablesUkrP

-- | Function 'sylLengths' shows number of Ukrainian letters (except 'ь') in the syllables in the text needed to represent a sounding of the text,
-- which was previously converted with 'syllablesUkr' function. If the syllable does not contain either sounds "дж" / "дз" or prolonged sounds then this number
-- is also a number of sounds in it.
-- If the Ukrainian word being written down contains an apostrophe or a dash (hyphen) signs (or even both) then they are treated as separators for the distinguished words.
-- This does not influence the syllable structure and so the poetic characteristics of the text.
sylLengths :: [[String]] -> [[Int]]
sylLengths = fmap (fmap (length . filter (/= 'ь')))

-- | Function 'sylLengthsP' shows number of 'UZP' in the syllables in the text,
-- which was previously converted with 'syllablesUkrP' function. If the syllable does not contain prolonged sounds then this number
-- is also a number of sounds in it.
-- If the Ukrainian word being written down contains an apostrophe or a dash (hyphen) signs (or even both) then they are treated as separators for the distinguished words.
-- This does not influence the syllable structure and so the poetic characteristics of the text.
sylLengthsP :: [[[UZP]]] -> [[Int]]
sylLengthsP = fmap (fmap length)

-- | Function 'sylLengthsP2' shows number of sounds in the syllables in the text,
-- which was previously converted with 'syllablesUkrP' function.
-- If the Ukrainian word being written down contains an apostrophe or a dash (hyphen) signs (or even both) then they are treated as separators for the distinguished words.
-- This does not influence the syllable structure and so the poetic characteristics of the text.
sylLengthsP2 :: [[[UZP]]] -> [[Int]]
sylLengthsP2 = fmap (fmap (length . representProlonged))

-- | Function 'representProlonged' is used internally in the 'sylLengthsP2' function. It converts duplicated consequent in the syllable consonants
-- so that they are represented by just one 'UZP'. After applying the function to the list of 'UZP' being a syllable all groups of duplicated consequent consonants
-- in every syllable are represented with only one 'UZP' respectively.
representProlonged :: [UZP] -> [UZP]
representProlonged (x:y:xs)
  | isVowel1 x = x:representProlonged (y:xs)
  | not . notEqC x $ y = y:representProlonged xs
  | otherwise = x:representProlonged (y:xs)
representProlonged xs = xs

-- | Is taken from the DobutokO.Sound.DIS5G6G module from @dobutokO2@ package.
-- See: 'https://hackage.haskell.org/package/dobutokO2-0.43.0.0/docs/DobutokO-Sound-DIS5G6G.html'
str2Durat1 :: String -> Float
str2Durat1 = getBFst' ((-0.153016), V.fromList [("-", (-0.101995)), ("0", (-0.051020)), ("1", (-0.153016)), ("а", 0.138231), ("б", 0.057143),
  ("в", 0.082268), ("г", 0.076825), ("д", 0.072063), ("дж", 0.048934), ("дз", 0.055601), ("е", 0.093605), ("ж", 0.070658), ("з", 0.056054),
    ("и", 0.099955), ("й", 0.057143), ("к", 0.045351), ("л", 0.064036), ("м", 0.077370), ("н", 0.074240), ("о", 0.116463), ("п", 0.134830),
      ("р", 0.049206), ("с", 0.074603), ("сь", 0.074558), ("т", 0.110658), ("у", 0.109070), ("ф", 0.062268), ("х", 0.077188), ("ц", 0.053061),
        ("ць", 0.089342), ("ч", 0.057596), ("ш", 0.066077), ("ь", 0.020227), ("і", 0.094150), ("ґ", 0.062948)])

-- | Returns list of lists, every inner one of which contains approximate durations of the Ukrainian syllables.
syllableDurations :: [[[UZP]]] -> [[Float]]
syllableDurations = fmap (fmap (sum . fmap (str2Durat1 . show) . representProlonged))
