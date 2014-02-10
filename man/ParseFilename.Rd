\name{ParseFilename}
\alias{ParseFilename}
\title{Extract information from a filename}
\usage{
ParseFilename(filename)
}
\arguments{
  \item{filename}{a filename with a pattern like
  [Task]_[BlockNo]_[SubjectID]}
}
\value{
a list with \code{Task}, \code{Block},\code{Subject} and
\code{Basename} fields.
}
\description{
The basename of a file in a Looking While Listening task
conforms to the naming convention:
[Task]_[BlockNo]_[SubjectID]. Block names are reduced to
just the integer value, i.e., \code{"Block1"} becomes
\code{1}.
}

