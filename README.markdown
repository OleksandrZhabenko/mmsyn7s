        ***** Usage *****
        =================

The package provides library functions and an executable. The executable
takes the first command line argument and (may be) a Ukrainian text
being written without quotes as the next command line arguments
and prints the sorted list of the Ukrainian sounds representations
(or something equivalent, see command line arguments) that can be used
further in mmsyn7 series of programs (or independently).

Depending on the first command line argument the program behaves as follows:

"-h" -- prints help and exits;

"-s" -- prints some general descriptive statistics metrices for the given text;

"-s2" -- prints syllable segmentation for the {number of words
(or their parts) to be taken for statistics, which is a second
command line argument} and some information about its structure
that can be interesting;

"-v" -- prints version number and exits;

"-1" -- prints the rest of the text after the first duplicated sound
representation (except silent ones) including it with whitespaces and
some phonetical conversions; 

"0"  -- prints the list of String for the whole text.

"1"  -- prints the list of String being unique (without silence);

"2"  -- prints the list of String for the whole text where every sound
representation is unique; 

"3"  -- prints the list of lists of Strings for the whole text, which
shows what sounds representations are needed to be created if every
sound representation is unique;

All other variants of the beginning for the command line arguments
are the same as "0" (the arguments are treated as a Ukrainian text
and processed as a whole one object).

                  ***** Syllable Segmentation *****
                  =================================

The program (and the library) can be used to obtain information about
syllable segmentation of the Ukrainian text. For more information,
please, refer to the above information about the "-s2" first command
line argument and to the documentation for the MMSyn7.Syllable module.

 ________________

The resulting Strings and lists of Strings can be used further in the
programs:

[mmsyn7ukr](https://hackage.haskell.org/package/mmsyn7ukr)

[mmsyn7h](https://hackage.haskell.org/package/mmsyn7h)

The results of the previous ones can be processed also by the program: 

[mmsyn7l](https://hackage.haskell.org/package/mmsyn7l)
