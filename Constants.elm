module Constants where
import Char (toCode, fromCode)

blockSize = 20

boardWidth = 11
boardHeight = boardWidth

restartKey = toCode 'r'

pauseKey = toCode 'p'

hardDropKey = toCode ' '

holdKey = toCode 'x'

toggleMusicKey = toCode 'm'

keys =
    { left = toCode 'h'
    , right = toCode 'l'
    , down = toCode 'j'
    , up = toCode 'k'
    }