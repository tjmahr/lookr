\name{AlignTrials}
\alias{AlignTrials}
\title{Align trial objects in a list}
\usage{
AlignTrials(trials, alignment_event = lwl_opts$get("alignment_event"))
}
\arguments{
  \item{trials}{}

  \item{alignment_event}{default is `"TargetOnset"`}
}
\value{
the inputted set of trials with normalized lengths (in
frame numbers), normalized `$Time` values (in ms.) and
updated timing attributes.
}
\description{
This function normalizes trials by length and by timing
such that:
}
\details{
\itemize{ \item all trials have the same number of frames
\item the event specified as time 0 occurs in the same
frame in each trial \item frames at the same index have the
same time (in ms.) across trials }

Because trials may vary in length, empty frames may be
added to the beginning or end of a trial to normalize the
frame-length of trials. These are filled with NA gaze
values. Trials also have updated timing attributes.
}

