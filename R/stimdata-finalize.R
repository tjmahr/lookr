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
  stimdata$FamiliarImage <- teleport_by_column(stimdata, "TargetImage")
  stimdata$UnfamiliarImage <- teleport_by_column(stimdata, "DistractorImage")
  stimdata$Target <- NULL

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
  img_stim <- c("TargetImage", "DistractorImage", "FamiliarImage", "UnfamiliarImage")

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

  # (1) Convert location numbers into location names using a lookup vector
  image_codes <- c(
    Image1 = "UpperLeftImage",
    Image2 = "UpperRightImage",
    Image3 = "LowerRightImage",
    Image4 = "LowerLeftImage"
  )
  sub_stim <- within(sub_stim, {
    # First four columns preserve the location of the Target/etc Image
    TargetImage           <- image_codes[Target]
    SemanticFoilImage     <- image_codes[SemanticFoil]
    PhonologicalFoilImage <- image_codes[PhonologicalFoil]
    UnrelatedImage        <- image_codes[Unrelated]
    Target           <- image_codes[Target]
    Unrelated        <- image_codes[Unrelated]
    SemanticFoil     <- image_codes[SemanticFoil]
    PhonologicalFoil <- image_codes[PhonologicalFoil]
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
  .GetStimImg <- function(j, df = sub_stim) {
    mapped <- teleport_by_column(df, j)
    str_replace(mapped, "\\d$", "")
  }

  sub_stim$Target <- .GetStimImg("Target")
  sub_stim$SemanticFoil <- .GetStimImg("SemanticFoil")
  sub_stim$PhonologicalFoil <- .GetStimImg("PhonologicalFoil")
  sub_stim$Unrelated <- .GetStimImg("Unrelated")

  # Merge these results back into the stimdata
  stimdata[names(sub_stim)] <- sub_stim

  # Specify the most important stimuli and combine them into the ordering
  first <- c("Task", "Protocol", "DateTime", "Subject", "Block", "TrialNo")
  ordering <- c(first, FOUR_PLACES, FOUR_TYPES, place_cols)

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
