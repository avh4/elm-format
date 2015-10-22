module Main (..) where

ratio =
    graphHeight
        / ( if range == 0 then
              0.1
            else
              toFloat range
          )


myTuple =
    (,,,)
        {- yes, this is valid! -} 1
        2
        3
        4
