module Integration where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = loop n 0 b -- iterations, area, currentX
  where -- accumulator += dx * (f(x) + f(x + dx)) / 2
    n = 1000 -- iterations
    dx = (b - a) / n
    loop 0 totalArea currX = totalArea
    loop i totalArea currX = loop (i - 1) (totalArea + area) x where
        area = dx * (y1 + y2) / 2
        y1 = f x
        y2 = f (x + dx)
        x = currX - dx
