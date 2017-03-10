module ForceField where

import Graphics.Gloss.Interface.Pure.Simulate

-- | Запустить демонстрацию.
demo :: IO ()
demo = simulate display bgColor fps initUniverse drawUniverse updateUniverse
  where
    display = InWindow "Flappy Lambda" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Модель вселенной.
data Universe = Universe

-- | Начальное состояние вселенной.
initUniverse :: Universe
initUniverse = Universe

-- | Отобразить вселенную.
drawUniverse :: Universe -> Picture
drawUniverse _ = blank

-- | Обновить состояние вселенной.
updateUniverse :: ViewPort -> Float -> Universe -> Universe
updateUniverse _ dt = id

-- | Ширина экрана.
screenWidth :: Num a => a
screenWidth = 500

-- | Высота экрана.
screenHeight :: Num a => a
screenHeight = 500
