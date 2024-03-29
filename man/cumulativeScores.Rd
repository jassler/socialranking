% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cumulative.R
\name{cumulativeScores}
\alias{cumulativeScores}
\alias{cumulativelyDominates}
\title{Cumulative scores}
\usage{
cumulativeScores(powerRelation, elements = powerRelation$elements)

cumulativelyDominates(powerRelation, e1, e2, strictly = FALSE)
}
\arguments{
\item{powerRelation}{A \code{\link{PowerRelation}} object created by \code{\link[=PowerRelation]{PowerRelation()}} or \code{\link[=as.PowerRelation]{as.PowerRelation()}}}

\item{elements}{Vector of elements of which to calculate their scores.
By default, the scores of all elements in \code{powerRelation$elements} are considered.}

\item{e1, e2}{Elements in \code{powerRelation$elements}}

\item{strictly}{If \code{TRUE}, check if \code{p1} \emph{strictly} dominates \code{p2}}
}
\value{
Score function returns a list of type \code{CumulativeScores} and length of \code{powerRelation$elements}
(unless parameter \code{elements} is specified).
Each index contains a vector of length \code{powerRelation$eqs}, cumulatively counting up the number of
times the given element appears in each equivalence class.

\code{cumulativelyDominates()} returns \code{TRUE} if \code{e1} cumulatively dominates \code{e2}, else \code{FALSE}.
}
\description{
Calculate cumulative score vectors for each element.
}
\details{
An element's cumulative score vector is calculated by cumulatively adding up the
amount of times it appears in each equivalence class in the \code{powerRelation}.
I.e., in a linear power relation with eight coalitions, if element 1 appears in coalitions placed at 1, 3, and 6,
its score vector is [1, 1, 2, 2, 2, 3, 3, 3].
}
\section{Dominance}{


\eqn{i}{i} dominates \eqn{j}{j} if, for each index
\eqn{x, \textrm{Score}(i)_x \geq \textrm{Score}(j)_x}{x, Score(i)_x >= Score(j)_x}.

\eqn{i}{i} \emph{strictly} dominates \eqn{j}{j} if there exists an \eqn{x}{x} such that
\eqn{\textrm{Score}(i)_x > \textrm{Score}(j)_x}{Score(i)_x > Score(j)_x}.
}

\examples{
pr <- as.PowerRelation("12 > 1 > 2")

# `1`: c(1, 2, 2)
# `2`: c(1, 1, 2)
cumulativeScores(pr)

# calculate for selected number of elements
cumulativeScores(pr, c(2))

# TRUE
d1 <- cumulativelyDominates(pr, 1, 2)

# TRUE
d2 <- cumulativelyDominates(pr, 1, 1)

# FALSE
d3 <- cumulativelyDominates(pr, 1, 1, strictly = TRUE)

stopifnot(all(d1, d2, !d3))

}
\references{
\insertRef{2015Cumulative}{socialranking}

\insertRef{2017axiomaticAndAlgorithmicPerspectives}{socialranking}
}
\seealso{
Other ranking solution functions: 
\code{\link{L1Scores}()},
\code{\link{L2Scores}()},
\code{\link{LPSScores}()},
\code{\link{LPScores}()},
\code{\link{copelandScores}()},
\code{\link{kramerSimpsonScores}()},
\code{\link{lexcelScores}()},
\code{\link{ordinalBanzhafScores}()}
}
\concept{ranking solution functions}
