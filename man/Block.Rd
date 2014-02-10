\name{Block}
\alias{Block}
\alias{Block.Gazedata}
\alias{Block.character}
\title{Combine gaze- and stimdata for an experimental Block}
\usage{
Block(...)

\method{Block}{character}(block_path)

\method{Block}{Gazedata}(gazedata, stimdata)
}
\arguments{
  \item{block_path}{A character string. The full file path
  of the gazedata and stimdata files (which should have the
  same directory path and basename) without the file
  extensions.}

  \item{gazedata}{A Gazedata object.}

  \item{stimdata}{A Stimdata object.}
}
\value{
A Block object---i.e., a list of trials, where each trial
is represented as a data.frame of its gazedata, augmented
with attributes for the stimdata.
}
\description{
Block is a generic function for initializing a Block
object, which represents the gazedata and stimdata for each
trial in an experimental block. Each trial is represented
as a data.frame of the gazedata with attributes for the
stimdata. The Block object is a list of trials.
}
\details{
Block.Gazedata is a method for initializing a Block object
from a Gazedata object and a Stimdata object.

Block.Stimdata is a method for initializing a Block object
from a Stimdata object and a Gazedata object. It's just
Block.Gazedata with the gazedata and stimdata parameters
reversed.

Block.character is a function for initializing a Block
object from the full file path of the block, excluding its
extension (since the extension is different for the
stimdata and gazedata files).
}

