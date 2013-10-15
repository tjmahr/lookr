\name{Session}
\alias{Session}
\alias{Session.character}
\alias{Session.list}
\title{Combine Blocks into Sessions}
\usage{
  Session(...)

  \method{Session}{list} (list_of_blocks)

  \method{Session}{character} (session_path)
}
\arguments{
  \item{list_of_blocks}{A list of Block objects, each Block
  object a block of the same experimental session. The
  Block objects in list_of_blocks should be ordered as they
  were presented during the experiment. That is, Block 1 of
  the experiment should be the first element of
  list_of_blocks, etc.}

  \item{session_path}{The directory path of both pairs of
  .gazedata and .txt files for the session---one pair for
  each block.}
}
\value{
  A Session object.
}
\description{
  Session is a generic function for initializing a Session
  object, which represents the gazedata and stimdata for
  each trial in an experimental session. The representation
  of each trial in a Session object is the same as in a
  Block object.
}
\details{
  Session.list is a method for initializing a Session
  object from a list of Block objects.

  Session.character is a method for initializing a Session
  object from the path of the directory that contains the
  .gazedata and .txt (stimdata) files for both blocks of an
  experimental session.
}

