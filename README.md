PREDATOR, PREY, AND A PLANT: DO CARPENTER BEES (XYLOCOPA CALIFORNICA) POSITION THEIR NESTS WITHIN SOTOL (DASYLIRION WHEELERI) LEAF ROSETTES TO MAXIMIZE PROTECTION FROM WOODPECKERS?

ABSTRACT--We examined a potential carpenter bee defense against nest predation by woodpeckers in southeastern Arizona. Desert carpenter bees (Xylocopa californica) build nests in dried inflorescence stalks of several plant species, including sotol (Dasylirion wheeleri). Sotol inflorescence stalks grow out of a dense rosette of barbed leaves. We asked whether nesting below these leaves protects carpenter bee larvae from woodpeckers. We recorded carpenter bee nest entrance height, the locations of bird predation marks, and the height of the tallest points of sotol leaf rosettes. Carpenter bee nests are placed preferentially in areas protected by sotol leaves. Evidence of predation also occurs significantly more often in protected regions of the stalk. However, nests that escape predation are placed deeper within the protection of the leaves. Our results are consistent with the hypothesis that sotol leaves may provide carpenter bee nests protection from woodpeckers.

What is here?

The scripts in this repository generate all the figures and analyses used in this article. Note: the illustrations combined with data in the published figures 1-4 are not included here, but the data portions of those figures are included.

How to execute these scripts:

You will need the raw data files 'Bird Pecks.csv' and 'Entrances.csv' (provided as Supplemental Materials separate from this repository). Save the .csv files in a directory titled 'data' in the same location as the scripts. Start with the data cleanup file, which will generate three tidy .csv files to use in subsequent scripts. The other scripts should then successfully run, drawing from the tidied .csv files generated from the previous step. This may be repeated for each of the figure scripts and question scripts. However, the scripted titled 'Entrance_Centering.R' requires a different raw data input file. This file is the one labled 'Entrances.csv.'

How to retrieve the output:

Each script saves a separate output file in a local directory titled 'Output'. Note: The script does not create the directory. The directory must exist prior to attempting this. Look for .png files for figures, and .txt files for statistical results.

Acknowledgements:

We thank the Bronstein Lab for valuable manuscript feedback, conceptual input, and methods suggestions; S.L. Buchmann for demonstrating carpenter bee identification, stalk collection and dissection methods, making suggestions to improve our study, and providing manuscript feedback; A.R. Cruz and L.C. Tellechea-Cerna for translating our abstract into Spanish; M.R. Frank for feedback on early writing stages of this project; J.C. Oliver (https://github.com/jcoliver) and J.K. Wilson (https://github.com/keatonwilson) for statistical and R consulting; O.L. Valenzuela who helped with data collection and transcription; G.J. Hughes for preliminary data on carpenter bee first tunneling direction and data transcription; M.M. Fosdick for helping with development and formatting of the project's poster; and the staff and board of the Southern Arizona Research, Science, and Engineering Foundation (SARSEF) who supported our initial collaboration on this study.

License:

We use the MIT license to dictate appropriate usage of this code.
