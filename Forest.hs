module Forest where 





import Data.Forest 

example :: Forest Char
example = forest
    [ tree 'a' $ leaves "bc"
    , tree 'd' $ forest
        [ leaf 'e'
        , tree 'f' $ leaves "g"
        ]
   ]