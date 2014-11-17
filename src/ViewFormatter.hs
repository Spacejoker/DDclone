module ViewFormatter where

import Model

hpString :: Player -> String
hpString p = (show $ pHealth p) ++ " / " ++ (show $ pMaxHealth p)

enemyHpString :: Enemy -> String
enemyHpString e = (show $ eHealth e) ++ "/" ++ (show $ eMaxHealth e)

