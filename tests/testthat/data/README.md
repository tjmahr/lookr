# Sample data from LWL experiments

Because our eye-tracking experiments develop through iteration and tweaking, `lookr` should be backwards-compatible---i.e., able to load and reduce data from *every* eyetracking experiment we've run using the looking-while-listening paradigm. That's the point of this directory. 

`lookr/inst/tests/data` collects two participants' worth of data from each experiment. These data are used to provide a comprehensive test of the package's functionality.

## Description of the experiments
The descriptions below provide details about stimulus presentation and timing for each experiment. 

### Mispronunciation

#### `MP_NoFixations_CS1`
The target words will have different durations but the carrier and silence are constant. Derive targetonset by taking AudioOnset and adding carrier duration and 80ms for silence.

#### `MP_WFFMovie_CS2a` and `MP_WFFMovie_CS2b`
These have two audio files, just CarrierOnset and TargetOnset. Target duration should all be the same. You can check that in praat.

#### `MP_WFFArea_Long` 
See above. 2 files, targets have same duration.

#### `MP_WFFArea_FourLevels`


### Coarticulation
The Coarticulation task was forked from `MP_WFFArea_Long`. It is a two-image task with a Wait-For-Fixation-Area stimulus presentation strategy. 

#### `Coartic_WFFArea_1a` and `Coartic_WFFArea_1b` (Dec 2012--Jan 2013)
These are pilot versions of the coarticulation experiment. Version `1b` merely has one-third fewer trials than `1a`. The audio stimuli are presented in two files:

1. **`CarrierStim`** (_find_, _see_): 970 ms
2. **`Target`** (_the ball_): 1270 ms. The noun following _the_ begins at 470 ms. 

Target-onset occurs 470 ms after the logged `Target.OnsetTime`. 

#### `Coartic_WFFArea_2a` (March 2013--ongoing)
The updated coarticulation pilot experiment incorporated "filler" trials and varied `Pitch` in the carrier phrase. There is just one `AudioStim` file (2190 ms), with the following parts:

1. **Carrier word** (_find_): 780 ms
2. **_the_**: 560 ms
3. **Target** (_ball_): 850 ms.

Target-onset occurs 1340 ms after `AudioStim.OnsetTime`.

### Real Word Listening

#### `RWL_NoFixations_CS1`
There are two version of this experiment, based on the dialect of the participant. 

Check the dialect durations.

##### SAE Version:



#### `RWL_WFFArea_Long`
Check the dialect durations. 
#### `RWL_WFFMovie_CS2a` and `RWL_WFFMovie_CS2b`

