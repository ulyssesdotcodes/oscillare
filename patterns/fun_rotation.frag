p $ pShapes "s" ((+ 3) . (* 5) . sinMod') 0.1 0.1
  |+| pRepeat 10
  |+| pScale' 0.5
  |+| pRotate ((* 3.1415) . (* 2))
  |+| pRepeat 4
  |+| pRotate (* (-3.1415))