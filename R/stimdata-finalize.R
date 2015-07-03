# Configure (image) stimuli according to on experimental task



# Functions to reorder stimdata columns.
ReorderStimdata <- function(...) UseMethod("ReorderStimdata")

ReorderStimdata.default <- function(stimdata, ordering = NULL) {
  leftovers <- setdiff(names(stimdata), ordering)
  stimdata <- stimdata[, c(ordering, leftovers)]
  stimdata
}

# By default just return the stimdata.
FinalizeStimdata <- function(...) UseMethod("FinalizeStimdata")
FinalizeStimdata.default <- function(stimdata) stimdata

FinalizeStimdata.MP <- function(stimdata) {
  classes <- class(stimdata)
  # In our analyses for MP task, the "target" image corresponds to the
  # "familiar" image. In the data, however, this is not the case, so we need to
  # flip the target-image location label for mispronounced and nonsense word
  # trials. In cheatsheet form:
  # ```
  #               Before correction:            After correction:
  #   StimType    Target        Distractor      Target        Distractor
  #   'real'      familiar      unfamiliar      familiar      unfamiliar
  #   'MP'        unfamiliar    familiar        familiar      unfamiliar
  #   'nonsense'  unfamiliar    familiar        familiar      unfamiliar
  # ```
  targets <- stimdata$Target
  flip_me <- stimdata$StimType %in% c("MP", "nonsense")
  targets <- ifelse(flip_me, flip_image(targets), targets)
  stimdata$TargetImage <- targets
  stimdata$DistractorImage <- flip_image(stimdata$TargetImage)

  stopifnot(all(stimdata$DistractorImage != stimdata$TargetImage),
            all(flip_me == (stimdata$Target != stimdata$TargetImage)))

  # Use the image filenames and the target image location to determine the
  # targeted word.
  .GetStimImg <- function(trial_num, col_name) {
    stim <- stimdata[trial_num, col_name]
    # Drop of the number at the end of the filename
    str_sub(stim, 1, -2)
  }

  stimdata <- transform(stimdata, Target = mapply(.GetStimImg, TrialNo, Target))

  # Filenames for audio files separate stimuli information with underscores:
  # `SAE_Fin_cat_112_20`. We reduce the filename down to the target word by
  # erasing numbers, dialect abbreviations, carrier phrase abbreviations and
  # underscores. We choose this strategy (erasure rather than extraction) in
  # case we cannot rely on the order of stimuli information.
  stimdata$TargetWord <- str_replace_all(stimdata$Audio,
                                         "[0-9]+|SAE|AAE|Fin|See|_+", "")

  # Reorder the stimdata
  first <- c("Task", "Protocol", "DateTime", "Subject", "Block", "TrialNo")
  target_stim <- c("TargetWord")
  place_stim <- c("ImageL", "ImageR")
  img_stim <- c("Target", "DistractorImage")

  ordering <- c(first, target_stim, place_stim, img_stim)
  stimdata <- ReorderStimdata(stimdata, ordering)
  class(stimdata) <- classes
  stimdata
}






FinalizeStimdata.RWL <- function(stimdata) {
  # Set some useful constants
  FOUR_PLACES <- c("UpperLeftImage", "UpperRightImage",
                   "LowerRightImage", "LowerLeftImage")
  FOUR_TYPES <- c("Target", "SemanticFoil", "PhonologicalFoil", "Unrelated")

  # Work with just the columns about images
  image_vars <- c("TrialNo", FOUR_PLACES, FOUR_TYPES)
  sub_stim <- stimdata[image_vars]

  # At this point, all we know is the trial number, the filename for each image
  # location, and the location number for each stimulus type. For example:
  # ```
  #   TrialNo         : int  1 2 3 4 5 6 7 8 9 10 ...
  #
  #   UpperLeftImage  : chr  "pan2" "bell2" "bee2" "box2" ...
  #   UpperRightImage : chr  "bell2" "kite2" "fly2" "drum2" ...
  #   LowerRightImage : chr  "drum2" "flag2" "clown2" "pan2" ...
  #   LowerLeftImage  : chr  "bee2" "comb2" "belt2" "van2" ...
  #
  #   SemanticFoil    : chr  "Image3" "Image3" "Image2" "Image1" ...
  #   PhonologicalFoil: chr  "Image4" "Image4" "Image4" "Image3" ...
  #   Unrelated       : chr  "Image1" "Image1" "Image3" "Image2" ...
  #   Target          : chr  "Image2" "Image2" "Image1" "Image4" ...
  # ```

  # To reduce the data, we (1) convert location number into location names, (2)
  # record what stimulus type occurs in each location for each trial, (3) map
  # the location names in the stimulus columns onto the image words (not the
  # filename). Output will look more like this:
  # ```
  #   TrialNo             : int  1 2 3 4 5 6 7 8 9 10 ...
  #
  #   UpperLeftImage      : chr  "pan2" "bell2" "bee2" "box2" ...
  #   UpperRightImage     : chr  "bell2" "kite2" "fly2" "drum2" ...
  #   LowerRightImage     : chr  "drum2" "flag2" "clown2" "pan2" ...
  #   LowerLeftImage      : chr  "bee2" "comb2" "belt2" "van2" ...
  #
  #   SemanticFoil        : chr  "drum" "flag" "fly" "box" ...
  #   PhonologicalFoil    : chr  "bee" "comb" "belt" "pan" ...
  #   Unrelated           : chr  "pan" "bell" "clown" "drum" ...
  #   Target              : chr  "bell" "kite" "bee" "van" ...
  #
  #   UpperLeftImagePlace : chr  "Unrelated" "Unrelated" "Target" ...
  #   UpperRightImagePlace: chr  "Target" "Target" "SemanticFoil" ...
  #   LowerRightImagePlace: chr  "SemanticFoil" "SemanticFoil" "Unrelated" ...
  #   LowerLeftImagePlace : chr  "PhonologicalFoil" "PhonologicalFoil" ...
  # ```

  # (1) Convert location numbers into location names.
  .GetImagePlace <- function(image) {
    switch(image,
           'Image1' = 'UpperLeftImage',
           'Image2' = 'UpperRightImage',
           'Image3' = 'LowerRightImage',
           'Image4' = 'LowerLeftImage')
  }
  # Vectorized version of the above function
  .GetImagePlaces <- function(col) sapply(col, .GetImagePlace)
  sub_stim <- within(sub_stim, {
    # TargetImage preserves the location of the Target Image
    TargetImage           <- .GetImagePlaces(Target)
    SemanticFoilImage     <- .GetImagePlaces(SemanticFoil)
    PhonologicalFoilImage <- .GetImagePlaces(PhonologicalFoil)
    UnrelatedImage        <- .GetImagePlaces(Unrelated)
    Target           <- .GetImagePlaces(Target)
    Unrelated        <- .GetImagePlaces(Unrelated)
    SemanticFoil     <- .GetImagePlaces(SemanticFoil)
    PhonologicalFoil <- .GetImagePlaces(PhonologicalFoil)
  })

  # (2) Record the type of stimulus that falls in each image location.
  .GetStimForPlace <- function(place, df = sub_stim) {
    # Initialize a vector to hold stimulus names
    num_stim <- nrow(df[place])
    place_stim <- character(num_stim)

    # For each stimulus type, extract the trials in which that stimulus type
    # occurs in `place`
    for (img in FOUR_TYPES) {
      rows <- which(df[img] == place)
      place_stim[rows] <- img
    }
    place_stim
  }
  place_cols <- paste0(FOUR_PLACES, "Stimulus")
  sub_stim[place_cols] <- Map(.GetStimForPlace, FOUR_PLACES)

  # (3) Assign image names onto stimulus types.
  .GetStimImg <- function(i, j) {
    stim <- sub_stim[i, j]
    str_sub(stim, 1, -2)
  }

  sub_stim <- within(sub_stim, {
    Target           <- mapply(.GetStimImg, TrialNo, Target)
    SemanticFoil     <- mapply(.GetStimImg, TrialNo, SemanticFoil)
    PhonologicalFoil <- mapply(.GetStimImg, TrialNo, PhonologicalFoil)
    Unrelated        <- mapply(.GetStimImg, TrialNo, Unrelated)
  })

  # Merge these results back into the stimdata
  stimdata[names(sub_stim)] <- sub_stim

  # Specify the most important stimuli and combine them into the ordering
  first <- c("Task", "Protocol", "DateTime", "Subject", "Block", "TrialNo")
  img_stim <- c("UpperLeftImageStimulus", "UpperRightImageStimulus",
                "LowerRightImageStimulus", "LowerLeftImageStimulus")

  ordering <- c(first, FOUR_PLACES, FOUR_TYPES, img_stim)

  stimdata <- ReorderStimdata(stimdata, ordering)

  stimdata
}




FinalizeStimdata.Coartic <- function(stimdata) {
  # Create a field for the Distractor Image
  stimdata$DistractorImage <- flip_image(stimdata$TargetImage)

  # Reorder the stimdata
  first <- c("Task", "DateTime", "Subject", "Block", "TrialNo")
  target_stim <- c("TargetWord")
  place_stim <- c("ImageL", "ImageR")
  img_stim <- c("TargetImage", "DistractorImage")

  ordering <- c(first, target_stim, place_stim, img_stim)
  stimdata <- ReorderStimdata(stimdata, ordering)

  stimdata
}


flip_image <- function(xs) ifelse(xs == "ImageL", "ImageR", "ImageL")
# flip_image(c("ImageL", "ImageR"))
