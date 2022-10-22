# Evidence weighting in confidence judgments for detection and discrimination
### Matan Mazor 👀, Lucie Charles 👀, Roni Maimon-Mor 👀& Stephen M. Fleming 👀

<img src="docs/figures/empirical_tile.png" alt="drawing" width="500"/>

## Data
Raw data from all three experiments is available on the project's OSF repository: [https://osf.io/7a4fm/](https://osf.io/7a4fm/)

## Analysis Scripts
Analysis scripts (in R) are available in the ['docs'](https://github.com/matanmazor/reverseCorrelation/blob/main/docs/reverseCorrelationPaper.Rmd) subdirectory.

## Experiment demos

You can try Experiment 2 by clicking [here](https://matanmazor.github.io/reverseCorrelation/experiments/demos/Experiment2/)

You can try Experiment 3 by clicking [here](https://matanmazor.github.io/reverseCorrelation/experiments/demos/Experiment3/)

You can try Experiment 4 by clicking [here](https://matanmazor.github.io/reverseCorrelation/experiments/demos/Experiment4/)

## Pre-registration time-locking 🕝🔒

To ensure preregistration time-locking (in other words, that preregistration preceded data collection), we employed [randomization-based preregistration](https://medium.com/@mazormatan/cryptographic-preregistration-from-newton-to-fmri-df0968377bb2). We used the SHA256 cryptographic hash function to translate our preregistered protocol folder (including the pre-registration document) to a string of 256 bits. These bits were then combined with the unique identifiers of single subjects, and the resulting string was used as seed for initializing the Mersenne Twister pseudorandom number generator prior to determining all random aspects of the experiment, including the order of trials, motion energy in Exp. 1, random luminance values in Exp 2 and 3, and hue values in Exp. 4. This way, experimental randomization was causally dependent on, and therefore could not have been determined prior to, the specific contents of our preregistration document ([Mazor, Mazor & Mukamel, 2019](https://doi.org/10.1111/ejn.14278)).

### Exp. 1
[protocol folder](https://github.com/matanmazor/reverseCorrelation/blob/main/experiments/Experiment1/protocolFolder.zip)

protocol sum: ba3759b8246ac88b1818437164bda3169334ea8f5e1e2a6b0b9af358cdb786ec

[relevant pre-registration lines of code](https://github.com/matanmazor/reverseCorrelation/blob/cbba2d43c2ddfb0c021ee0c15b7d5b03eddd34d8/experiments/Experiment1/loadPars.m#L35-L39)

### Exp. 2
[protocol folder](https://github.com/matanmazor/reverseCorrelation/blob/cbba2d43c2ddfb0c021ee0c15b7d5b03eddd34d8/experiments/Experiment2/protocol_folder.zip)

protocol sum: f078aa2862041786868ef9f2ad23336df49386eb98fe8259951830bdf7c3dbfd

[relevant pre-registration lines of code](https://github.com/matanmazor/reverseCorrelation/blob/cbba2d43c2ddfb0c021ee0c15b7d5b03eddd34d8/experiments/Experiment2/webpage/ZylbRep.html#L677-L687)

### Exp. 3
[protocol folder](https://github.com/matanmazor/reverseCorrelation/blob/cbba2d43c2ddfb0c021ee0c15b7d5b03eddd34d8/experiments/Experiment3/protocol_folder.zip)

protocol sum: c8c398e9134c072a7c73ea6a24f87079609999df2a40e52c19f94e3d98a58d2c;

[relevant pre-registration lines of code](https://github.com/matanmazor/reverseCorrelation/blob/cbba2d43c2ddfb0c021ee0c15b7d5b03eddd34d8/experiments/Experiment3/webpage/main.js#L682-L692)

### Exp. 4
[protocol folder](https://github.com/matanmazor/reverseCorrelation/blob/main/experiments/Experiment4/protocol_folder.zip)

protocol sum: 099327c4129fd85ced7a97fd6b6e131596e3a1569f641a50435c545755342ce8

[relevant pre-registration lines of code](https://github.com/matanmazor/reverseCorrelation/blob/6d012473f41ddfc890e6f0d559b60d061b65055d/experiments/Experiment4/webpage/main.js#L806-L822)
