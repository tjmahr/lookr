\name{TryLoad}
\alias{TryLoad}
\title{Try to load an experimental block, failing gracefully if unsuccessful}
\usage{
TryLoad(subject_path, handler = warning)
}
\arguments{
  \item{subject_path}{a path to a subject's directory which
  may contain blocks from LWL tasks.}

  \item{handler}{a function to apply to the file-path of an
  experimental block hat raised an error. The default is to
  `print` the file-path.}
}
\value{
`NULL` if there are no loadable experimental blocks in
`subject_path`, otherwise a `Session` containing the blocks
that could be loaded from `subject_path`.
}
\description{
`TryLoad(...)` tries to load the blocks in a subject
directory. If it encounters an error, it catches the error
and moves on to the next in the subject directory.
}

