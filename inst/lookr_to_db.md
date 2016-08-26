Normalizing Eyetracking Data
================
Tristan Mahr
October 22, 2015

Gameplan
--------

Our goal today is to prepare our eyetracking data to be inserted into a database.

For database normalization, we want there to be five tables:

-   Experiments: 1 row per administration of experiment.
-   Trials: 1 row per trial of an eye-tracking experiment.
-   Frames: 1 row per frame of eyetracking data in a trial.
-   ExperimentsInfo: 1 row per attribute of a experiment
-   TrialInfo: 1 row per attribute of a trial

The first three tables are meant to be nice and nested "wide" dataframes. We have looks nested in trials nested in experiment administrations, and each row is one thing (a frame, a trial, an experiment). These three tables are experiment agnostic.

The other tables are meant to be a grab-bag of key-value pairs, containing things like the target word of each trial or the dialect of the experiment administration. These will be "long" dataframes, such that there are multiple rows for a single trial or experiment. The specific attributes that differentiate trials and experiments live in these tables. These tables will allow the other three to be completely task agnostic.

Eyetracking data from lookr
---------------------------

Use lookr to load and reduce some eye-tracking data.

``` r
library("knitr")
library("lookr")
library("dplyr") # manipulate tables
library("tidyr") # wide/long tables

opts_chunk$set(comment = "#>", collapse = TRUE)
```

``` r
# Load some example data bundled in lookr
mp_long <- file.path(find.package("lookr"), "docs/data/MP_WFFArea_Long/")
trials <- suppressMessages(Task(mp_long))
trials
#> Task object with 2 Subject IDs and 144 trials: 
#>   SubjectID            Basename Trials
#> 1 001P00XA1 MP_Block1_001P00XA1     36
#> 2 001P00XA1 MP_Block2_001P00XA1     36
#> 3 001P00XS1 MP_Block1_001P00XS1     36
#> 4 001P00XS1 MP_Block2_001P00XS1     36

# Light preprocessing: Set time 0 to target onset, map gaze locations to AOIs,
# interpolate spans of missing data up to 150ms in duration
trials <- AdjustTimes(trials, "TargetOnset")
# Can save a lot of space by just keep the looks from after the speech starts
trials <- TimeSlice(trials, from = "CarrierOnset")
trials <- AddAOIData(trials)
trials <- InterpolateMissingFrames(trials)

# Perform a bias calculation as in the RWL paper. (Measures the most viewed
# image during a time window.)
bias_window <- c(0, 245)
trials <- CalculateBias(trials, bias_window)
```

Note that each `Trial` object is just `data.frame` with several associated `attributes`.

``` r
print(trials[[1]], width = 80, strict.width = "wrap")
#> Classes 'Trial' and 'data.frame':    181 obs. of  20 variables:
#> $ Task : chr "MP" "MP" "MP" "MP" ...
#> $ Subject : chr "001P00XA1" "001P00XA1" "001P00XA1" "001P00XA1" ...
#> $ BlockNo : int 1 1 1 1 1 1 1 1 1 1 ...
#> $ Basename : chr "MP_Block1_001P00XA1" "MP_Block1_001P00XA1"
#>    "MP_Block1_001P00XA1" "MP_Block1_001P00XA1" ...
#> $ TrialNo : int 1 1 1 1 1 1 1 1 1 1 ...
#> $ Time : num -983 -966 -949 -933 -916 ...
#> $ XLeft : num 0.796 0.81 0.799 0.807 0.8 ...
#> $ XRight : num 0.806 0.804 0.802 0.803 0.808 ...
#> $ XMean : num 0.801 0.807 0.801 0.805 0.804 ...
#> $ YLeft : num 0.566 0.547 0.564 0.561 0.55 ...
#> $ YRight : num 0.533 0.521 0.544 0.53 0.539 ...
#> $ YMean : num 0.55 0.534 0.554 0.545 0.545 ...
#> $ ZLeft : num 595 595 594 594 595 ...
#> $ ZRight : num 598 598 598 598 598 ...
#> $ ZMean : num 596 596 596 596 596 ...
#> $ DiameterLeft : num 2.96 2.96 2.91 2.94 2.93 ...
#> $ DiameterRight : num 2.96 2.95 2.93 2.93 2.9 ...
#> $ DiameterMean : num 2.96 2.96 2.92 2.93 2.92 ...
#> $ GazeByAOI : chr "ImageR" "ImageR" "ImageR" "ImageR" ...
#> $ GazeByImageAOI: chr "Target" "Target" "Target" "Target" ...
#> - attr(*, "Task")= chr "MP"
#> - attr(*, "Protocol")= chr "WFF_Area"
#> - attr(*, "DateTime")= chr "2013-01-21 08:55:16"
#> - attr(*, "Subject")= chr "001P00XA1"
#> - attr(*, "Block")= num 1
#> - attr(*, "TrialNo")= num 1
#> - attr(*, "TargetWord")= chr "girl"
#> - attr(*, "ImageL")= chr "marmoset1"
#> - attr(*, "ImageR")= chr "girl1"
#> - attr(*, "TargetImage")= chr "ImageR"
#> - attr(*, "DistractorImage")= chr "ImageL"
#> - attr(*, "FamiliarImage")= chr "girl1"
#> - attr(*, "UnfamiliarImage")= chr "marmoset1"
#> - attr(*, "WordGroup")= chr "girl"
#> - attr(*, "StimType")= chr "real"
#> - attr(*, "ImageOnset")= num -3056
#> - attr(*, "Audio")= chr "AAE_girl_312_10"
#> - attr(*, "Attention")= chr "AAE_check2_10"
#> - attr(*, "AttentionDur")= num 914
#> - attr(*, "CarrierOnset")= num -968
#> - attr(*, "FixationOnset")= num -1503
#> - attr(*, "DelayTargetOnset")= num 8
#> - attr(*, "TargetOnset")= num 0
#> - attr(*, "TargetDur")= num 712
#> - attr(*, "CarrierDur")= num 960
#> - attr(*, "AttentionOnset")= num 1720
#> - attr(*, "Dialect")= chr "AAE"
#> - attr(*, "AttentionEnd")= num 2634
#> - attr(*, "FixationDur")= num 535
#> - attr(*, "CarrierEnd")= num -8
#> - attr(*, "TargetEnd")= num 712
#> - attr(*, "Basename")= chr "MP_Block1_001P00XA1"
#> - attr(*, "FrameRate")= num 16.7
#> - attr(*, "AlignedBy")= chr "TargetOnset"
#> - attr(*, "NumberOfFrames")= int 181
#> - attr(*, "InterpolatedPoints")= num 11
#> - attr(*, "CorrectedFrames")= num 14 18 51 52 53 111 112 113 172 173 ...
#> - attr(*, "CorrectedTimes")= num -766 -699 -150 -133 -117 ...
#> - attr(*, "InterpolationWindow")= num 150
#> - attr(*, "Bias")= chr "Distractor"
#> - attr(*, "BiasSummary")='data.frame': 1 obs. of 6 variables:
#> ..$ GazeByImageAOI: chr "Distractor"
#> ..$ Frames : int 15
#> ..$ Weight : num 3747
#> ..$ FirstLook : num 0
#> ..$ Earliness : num 233
#> ..$ Bias : num 3980
```

Extracting just the data-frame parts and combining them will provide us with the rows for the Frames table. The Experiments/Trials rows will come from the attributes.

To link trials to administrations, we will create a new attribute `TrialName` attibute that will uniquely identify trials.

``` r
# Uniquely name each trial
trials %@% "TrialName" <- 
  sprintf("%s_%02.0f", trials %@% "Basename", trials %@% "TrialNo")
```

Creating the wide tables
------------------------

Now we can create the three main "wide" (task-invariant) tables. We `Basename` to link experiment administrations to `TrialName` and `TrialName` to link to eye-tracking frames.

``` r
tbl_exps <- trials %>% 
  gather_attributes(c("Basename", "DateTime", "Subject", "Task")) %>% 
  as_data_frame %>% 
  distinct
tbl_exps
#> # A tibble: 4 x 4
#>              Basename            DateTime   Subject  Task
#>                 <chr>               <chr>     <chr> <chr>
#> 1 MP_Block1_001P00XA1 2013-01-21 08:55:16 001P00XA1    MP
#> 2 MP_Block2_001P00XA1 2013-03-13 09:01:31 001P00XA1    MP
#> 3 MP_Block1_001P00XS1 2012-10-17 09:06:47 001P00XS1    MP
#> 4 MP_Block2_001P00XS1 2012-11-16 09:12:05 001P00XS1    MP

tbl_trials <- trials %>% 
  gather_attributes(c("TrialName", "Basename", "TrialNo")) %>% 
  as_data_frame %>% 
  distinct
tbl_trials
#> # A tibble: 144 x 3
#>                 TrialName            Basename TrialNo
#>                     <chr>               <chr>   <dbl>
#> 1  MP_Block1_001P00XA1_01 MP_Block1_001P00XA1       1
#> 2  MP_Block1_001P00XA1_02 MP_Block1_001P00XA1       2
#> 3  MP_Block1_001P00XA1_03 MP_Block1_001P00XA1       3
#> 4  MP_Block1_001P00XA1_04 MP_Block1_001P00XA1       4
#> 5  MP_Block1_001P00XA1_05 MP_Block1_001P00XA1       5
#> 6  MP_Block1_001P00XA1_06 MP_Block1_001P00XA1       6
#> 7  MP_Block1_001P00XA1_07 MP_Block1_001P00XA1       7
#> 8  MP_Block1_001P00XA1_08 MP_Block1_001P00XA1       8
#> 9  MP_Block1_001P00XA1_09 MP_Block1_001P00XA1       9
#> 10 MP_Block1_001P00XA1_10 MP_Block1_001P00XA1      10
#> # ... with 134 more rows

# Make a function that extracts the columns of gaze data from a Trial, then 
# apply it to each Trial
collect_looks <- function(x) {
  x_name <- x %@% "TrialName"
  # c(x) to strip attributes
  x <- c(x) %>% 
    as_data_frame %>% 
    mutate(TrialName = x_name) %>% 
    select(TrialName, Time, XMean, YMean, GazeByImageAOI, GazeByAOI)
  x
}

looks <- trials %>% purrr::map_df(collect_looks)

# Convert screen proportions to pixel locations
tbl_looks <- looks %>% 
  mutate(XMean = round(XMean * lwl_constants$screen_width),
         YMean = round(YMean * lwl_constants$screen_height))
pryr::object_size(tbl_looks)
#> 1.34 MB
tbl_looks
#> # A tibble: 27,736 x 6
#>                 TrialName      Time XMean YMean GazeByImageAOI GazeByAOI
#>                     <chr>     <dbl> <dbl> <dbl>          <chr>     <chr>
#> 1  MP_Block1_001P00XA1_01 -982.6214  1538   660         Target    ImageR
#> 2  MP_Block1_001P00XA1_01 -965.9668  1549   641         Target    ImageR
#> 3  MP_Block1_001P00XA1_01 -949.3122  1538   665         Target    ImageR
#> 4  MP_Block1_001P00XA1_01 -932.6576  1545   654         Target    ImageR
#> 5  MP_Block1_001P00XA1_01 -916.0030  1545   654         Target    ImageR
#> 6  MP_Block1_001P00XA1_01 -899.3484  1542   661         Target    ImageR
#> 7  MP_Block1_001P00XA1_01 -882.6938  1536   657         Target    ImageR
#> 8  MP_Block1_001P00XA1_01 -866.0392  1547   647         Target    ImageR
#> 9  MP_Block1_001P00XA1_01 -849.3846  1285   656         Target    ImageR
#> 10 MP_Block1_001P00XA1_01 -832.7300  1048   669        tracked   tracked
#> # ... with 27,726 more rows
```

Creating the grab-bag tables
----------------------------

### Prepare experimental grab-bag and flatten AOI definitions

Now we need to make the grab-bags of trial and experiment attributes. But first let's unpack the implicit AOI definitions used in the experiments.

``` r
library("tidyr")
library("stringr")

# hard-coded inside lookr:::GetImageAOI
AOIs <- list(
  UpperLeftImage = list(x_pix = c(410, 860), y_pix = c(700, 1150)), 
  LowerLeftImage = list(x_pix = c(410, 860), y_pix = c(50, 500)), 
  UpperRightImage = list(x_pix = c(1060, 1510), y_pix = c(700, 1150)), 
  LowerRightImage = list(x_pix = c(1060, 1510), y_pix = c(50, 500)), 
  FixationImage = list(x_pix = c(885, 1035), y_pix = c(525, 675)), 
  ImageL = list(x_pix = c(100, 700), y_pix = c(300, 900)), 
  ImageR = list(x_pix = c(1220, 1820), y_pix = c(300, 900)))

# Break apart names from unlist(...) into separate columns
aoi_table <- unlist(AOIs) %>% 
  data_frame(Image = names(.), Pixel = .) %>% 
  separate(Image, into = c("AOI", "Dimension", "Number")) %>% 
  mutate(Number = str_replace(Number, "pix", ""),
         AOIBoundary = "AOI_Boundary")
  
# Keep only values used in the data and combine columns into Key, Value
aoi_table <- aoi_table %>% 
  filter(AOI %in% unique(looks$GazeByAOI)) %>% 
  unite(col = AOI_Boundary, AOIBoundary, AOI:Number) %>% 
  rename(Key = AOI_Boundary, Value = Pixel)
aoi_table
#> # A tibble: 8 x 2
#>                       Key Value
#> *                   <chr> <dbl>
#> 1 AOI_Boundary_ImageL_x_1   100
#> 2 AOI_Boundary_ImageL_x_2   700
#> 3 AOI_Boundary_ImageL_y_1   300
#> 4 AOI_Boundary_ImageL_y_2   900
#> 5 AOI_Boundary_ImageR_x_1  1220
#> 6 AOI_Boundary_ImageR_x_2  1820
#> 7 AOI_Boundary_ImageR_y_1   300
#> 8 AOI_Boundary_ImageR_y_2   900
```

Now we assembly the experiment key-value grab-bag and attach the AOI definitions.

``` r
# Gather experiment attributes
tbl_exps_gb <- trials %>% 
  gather_attributes(c("Basename", "Dialect", "Protocol")) %>% 
  as_data_frame %>% 
  distinct %>% 
  # Other hard-wired constants
  mutate(FrameRate = lwl_constants$ms_per_frame,
         ScreenWidth = lwl_constants$screen_width,
         ScreenHeight = lwl_constants$screen_height) %>% 
  gather(key = Key, value = Value, -Basename) 
# 
# # Create all combinations of basename and AOI property name
# aoi_rows <- list(Basename = tbl_exps$Basename, Key = aoi_table$Key) %>% 
#   cross_n %>% 
#   bind_rows %>% 
#   left_join(aoi_table) %>% 
#   mutate(Value = as.character(Value))
# aoi_rows
# 
# # Combine AOI attributes with other ones
# tbl_exps_attrs <- bind_rows(tbl_exps_gb, aoi_rows) %>% 
#   arrange(Basename, Key)
tbl_exps_attrs <- tbl_exps_gb
tbl_exps_attrs
#> # A tibble: 20 x 3
#>               Basename          Key    Value
#>                  <chr>        <chr>    <chr>
#> 1  MP_Block1_001P00XA1      Dialect      AAE
#> 2  MP_Block2_001P00XA1      Dialect      AAE
#> 3  MP_Block1_001P00XS1      Dialect      SAE
#> 4  MP_Block2_001P00XS1      Dialect      SAE
#> 5  MP_Block1_001P00XA1     Protocol WFF_Area
#> 6  MP_Block2_001P00XA1     Protocol WFF_Area
#> 7  MP_Block1_001P00XS1     Protocol WFF_Area
#> 8  MP_Block2_001P00XS1     Protocol WFF_Area
#> 9  MP_Block1_001P00XA1    FrameRate  16.6546
#> 10 MP_Block2_001P00XA1    FrameRate  16.6546
#> 11 MP_Block1_001P00XS1    FrameRate  16.6546
#> 12 MP_Block2_001P00XS1    FrameRate  16.6546
#> 13 MP_Block1_001P00XA1  ScreenWidth     1920
#> 14 MP_Block2_001P00XA1  ScreenWidth     1920
#> 15 MP_Block1_001P00XS1  ScreenWidth     1920
#> 16 MP_Block2_001P00XS1  ScreenWidth     1920
#> 17 MP_Block1_001P00XA1 ScreenHeight     1200
#> 18 MP_Block2_001P00XA1 ScreenHeight     1200
#> 19 MP_Block1_001P00XS1 ScreenHeight     1200
#> 20 MP_Block2_001P00XS1 ScreenHeight     1200
```

### Prepare trial grab-bag

Now we can do the trials attributes.

``` r
# Collect the attributes that have only one value for a trial
trial_attrs <- 
  c("TrialName", "StimType", "WordGroup", "TargetWord",
    "TargetImage", "DistractorImage", "ImageL", "ImageR", 
    "FamiliarImage", "UnfamiliarImage", 
    "Audio", "Attention", "InterpolationWindow",
    # trial events
    "ImageOnset",
    "FixationOnset", "FixationDur", 
    "CarrierOnset", "CarrierEnd",
    "TargetOnset", "TargetEnd", 
    "AttentionOnset", "AttentionEnd")

tbl_trial_attrs_gb <- trials %>% 
  gather_attributes(trial_attrs, omit_na = TRUE) %>% 
  as_data_frame %>% 
  gather(Key, Value, -TrialName) %>% 
  mutate(Key = as.character(Key))

# Collect the times of frames that underwent interpolation. Since there are 
# zero-to-many such frames for a trial, handle these separately. Use a function
# to solve problem for one Trial, then map to all trials
get_corrected_times <- function(x) {
  corrections <- x %@% "CorrectedTimes"
  names(corrections) <- rep("InterpolatedTime", length(corrections))
  data_frame(
    TrialName = rep(x %@% "TrialName", length(corrections)),
    Key = names(corrections),
    Value = as.character(corrections))
}

interpolated_times <- trials %>% 
  purrr::map_df(get_corrected_times)

# Do something similar with the bias measurements. Create a dataframe
# summarizing which AOI was viewed the most/earliest during the bias window.
get_bias_measures <- function(x, window) {
  bias_df <- x %@% "BiasSummary"
  
  if (nrow(bias_df) != 0) {
    # If eyetracking data available, return which image had most bias
    bias_df <- bias_df %>% 
      filter(Bias == max(Bias)) %>% 
      select(Bias_ImageAOI = GazeByImageAOI, Bias_FrameCount = Frames)
  } else {
    # Otherwise, give NAs
    bias_df <- data_frame(Bias_ImageAOI = NA, Bias_FrameCount = NA)
  }
  
  bias_df <-  bias_df %>% 
    mutate(TrialName = x %@% "TrialName", 
           Bias_WindowStart = min(window), 
           Bias_WindowEnd = max(window)) 
  bias_df
}

bias_summary <- trials %>% 
  purrr::map_df(get_bias_measures, window = bias_window) %>% 
  gather(Key, Value, -TrialName)

tbl_trial_attrs <- tbl_trial_attrs_gb %>% 
  bind_rows(interpolated_times, bias_summary) %>% 
    arrange(TrialName, Key, Value)
pryr::object_size(tbl_trial_attrs)
#> 164 kB
tbl_trial_attrs
#> # A tibble: 5,059 x 3
#>                 TrialName              Key           Value
#>                     <chr>            <chr>           <chr>
#> 1  MP_Block1_001P00XA1_01        Attention   AAE_check2_10
#> 2  MP_Block1_001P00XA1_01     AttentionEnd            2634
#> 3  MP_Block1_001P00XA1_01   AttentionOnset            1720
#> 4  MP_Block1_001P00XA1_01            Audio AAE_girl_312_10
#> 5  MP_Block1_001P00XA1_01  Bias_FrameCount              15
#> 6  MP_Block1_001P00XA1_01    Bias_ImageAOI      Distractor
#> 7  MP_Block1_001P00XA1_01   Bias_WindowEnd             245
#> 8  MP_Block1_001P00XA1_01 Bias_WindowStart               0
#> 9  MP_Block1_001P00XA1_01       CarrierEnd              -8
#> 10 MP_Block1_001P00XA1_01     CarrierOnset            -968
#> # ... with 5,049 more rows

tbl_trial_attrs %>% distinct(Key)
#> # A tibble: 26 x 1
#>                 Key
#>               <chr>
#> 1         Attention
#> 2      AttentionEnd
#> 3    AttentionOnset
#> 4             Audio
#> 5   Bias_FrameCount
#> 6     Bias_ImageAOI
#> 7    Bias_WindowEnd
#> 8  Bias_WindowStart
#> 9        CarrierEnd
#> 10     CarrierOnset
#> # ... with 16 more rows
```
