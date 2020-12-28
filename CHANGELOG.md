# Revision history for mmsyn7s

## 0.1.0.0 -- 2020-01-09

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2020-01-10

* First version revised A. Added a function show7s2 for the usage in the mmsyn7ukr package.

## 0.2.0.0 -- 2020-02-24

* Second version. Added functions that can be used to divide a given text to parts with the unique Ukrainian sounds representations (except silent ones) and
the rest of the text being represented. Changed code for mmsyn7s function for readability. Some minor documentation improvements.

## 0.3.0.0 -- 2020-02-25

* Third version. Added README.markdown file. Added the analysis for the first command line argument. Added the possibility to process a Ukrainian text as a whole
or alternatively by parts, each of which contains only unique String Ukrainian sounds representations (except for the silent ones). Some minor code and documentation
improvements.

## 0.3.0.1 -- 2020-02-25

* Third version revised A. Improved formatting for the README.markdown file.

## 0.3.1.0 -- 2020-02-25

* Third version revised B. Separated the "1" option into two different ones: "1" and "-1", each of which prints either a list of Strings or the rest of the text being
treated partially till the first duplicated Ukrainian sound representation (except the silent ones). Improved formatting for the documentation.

## 0.3.1.1 -- 2020-02-25

* Third version revised C. Fixed issue with being not complete the "-h" first command line argument information.

## 0.4.0.0 -- 2020-02-26

* Fourth version. Fixed issue with the documentation for the 'show7s5' function. Added some new library functions. Added the possibility to get some statistic metrics
and to obtain a list of lists of Strings -- the information about how many and what actually Ukrainian sounds representations are needed to be created if every sound
representation is unique.

## 0.5.0.1 -- 2020-02-26

* Fifth version revised A. Fixed issue with the duplicated record in the statistics. Improved formatting for the statistics printing by the program.

## 0.5.0.2 -- 2020-02-26

* Fifth version revised B. Fixed issue with the version number.

## 0.6.0.0 -- 2020-02-28

* Sixth version. Added packages mmsyn2 and mmsyn5 as a dependencies explicitly (earlier they wele as implicit dependencies because they are dependencies for
mmsyn6ukr). Added the module MMSyn7.Syllable and the instruments to work with syllables. Added ghc-options for mmsyn7s.cabal file. Added some performance
improvements to the previous code. Some grammar issues fixed.

## 0.6.1.0 -- 2020-02-28

* Sixth version revised A. Fixed issue with the not being compiled because of the deprecated information in the mmsyn7s.cabal file.

## 0.6.2.0 -- 2020-02-28

* Sixth version revised B. Fixed issue with the not being compiled because of the wrong application of the function in the 'exceptRead' function.

## 0.6.3.0 -- 2020-02-28

* Sixth version revised C. Fixed issue with the not being compiled because of the wrong application of the function in the 'takeWordS' function.

## 0.6.4.0 -- 2020-02-29

* Sixth version revised D. Fixed issue with the 'notEqC' and respectively 'divideConsonants' functions. Added new functions to deal more with UZP datatype and so with
classical sounds in the MMSyn7.Syllable module and respectively in the MMSyn7s and Main modules. Changed the behaviour for "-s2" first command line argument
for mmsyn7s executable. Some minor documentation improvements.

## 0.6.5.0 -- 2020-03-01

* Sixth version revised E. Improved the performance of the 'main7s' function for the "-s" first command line parameter.

## 0.6.5.1 -- 2020-03-01

* Sixth version revised F. Improved formatting for the statistics printing for "-s" and "-s2" first command line arguments.

## 0.6.6.0 -- 2020-03-08

* Sixth version revised G. Changed the import of the datatype 'UZP' from the module MMSyn7.Syllable.

## 0.6.7.0 -- 2020-05-14

* Sixth version revised H. Changed the bounds for dependencies so that now also GHC 8.10* series are supported.

## 0.7.0.0 -- 2020-06-24

* Seventh version. Changed Double to Float in the MMSyn7s module so that the functons can be consistent with new dobutokO packages. Besides, the double precision
is not needed and in some cases is meaningless and can potentially (a little bit, however) reduce performance in some cases. Some minor code and documentation improvements.

## 0.8.0.0 -- 2020-08-16

* Eighth version. Changed the import from MMSyn6Ukr.Show7s module and therefore the mmsyn6ukr dependency bounds.

## 0.9.0.0 -- 2020-10-08

* Ninth version. Added new functions to evaluate syllables durations.

## 0.9.1.0 -- 2020-10-09

* Ninth version revised A. Added additionalF to the export in the module MMSyn7.Syllable.
