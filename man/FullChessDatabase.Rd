\name{FullChessDatabase}
\alias{FullChessDatabase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{An R Database of 1.1 Million Chess Openings}
\description{
This database contains annually-resolved, individual-level data on Chess opening moves, from more than 1.1 million games, played by more than 54,000 rated Chess players. 
}
\usage{
data(FullChessDatabase)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
There are two data frames that are loaded, R and R200. R is the full database. R200 is a subset of R which included the full records of Chess player with at least 200 recored games. R200 was the subset of individual level data used in the Evolution and Human Behavior paper.
\enumerate{
\item Date : Year the game was played.
\item EloW : Elo rating of the player with white pieces.
\item EloB : Elo rating of the player with black pieces.
\item NameWhite : Name of the player with white pieces.
\item NameBlack : Name of the player with black pieces.
\item Win : Categorical variable. 1==win by player with white pieces. 2==draw. 3==win by player with black pieces.
\item X1...X10 : X1==first move of game. X2==second move of game (first move by black). X3==third move of game (second move by white). Etc.
}
}
\value{
}
\references{}
\seealso{}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
