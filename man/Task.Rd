\name{Task}
\alias{Task}
\title{Load the sessions for an experimental task}
\usage{
Task(...)
}
\arguments{
  \item{task_path}{a task-level directory. The
  subdirectories of `task_path` should be subject
  directories, that begin with the pattern
  `[0-9]{3}[CLPX]`, and these subject directories should
  contain experimental blocks.}

  \item{partial}{a vector of integers used to select some
  of the subdirectories. Used to partially load the set of
  sessions for the task, in order to prototype code or test
  the function. Default is `NA`.}

  \item{handler}{a function used to handle error messages.
  By default, an error message is caught and printed as a
  `warning`.}
}
\value{
a Task object containing all the trials from all the
sessions that could be successfully loaded. If a block
could not be loaded, its error message is handled using the
specified handle function.
}
\description{
`Block(...)` collects the trials for a single
looking-while-listening experiment. `Session(...)` combines
blocks for a single subject. `Task(...)` combines sessions
over multiple subjects.
}
\details{
A `Task` object is a list of `Trial` objects, which all
have the same `"Task"` attribute but do not have the same
`"Subject"` attribute.
}

