module ForceField where

import Data.Monoid
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Vector

-- | Запустить демонстрацию.
demo :: [Body] -> IO ()
demo bodies = simulate display bgColor fps (initUniverse bodies) drawUniverse updateUniverse
  where
    display = InWindow "Flappy Lambda" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 240     -- кол-во кадров в секунду

-- | Солнечная система с двумя звёздами в центре и двумя планетами,
-- у каждой из которых есть один спутник.
sampleBinaryStarSystem :: [Body]
sampleBinaryStarSystem = [ sun1, sun2, earth1, moon1, earth2, moon2 ]
  where
    sun1  = orbitingAt ( 15, 0) (Body (5, 0) (0, 0) 12.5) 100
    sun2  = orbitingAt (-15, 0) (Body (5, 0) (0, 0) 25) 50

    earth1 = orbitingAt (100, 150) (sun1 <> sun2) 10
    moon1  = orbitingAt (5, 10) earth1 1

    earth2 = orbitingAt (-200, 0) (sun1 <> sun2) 15
    moon2  = orbitingAt (-5, -10) earth2 2

    orbitingAt pos body mass = Body point (orbitVelocity body point) mass
      where
        point = bodyPosition body + pos

-- | Модель вселенной.
data Universe = Universe
  { universeBodies :: [Body]
  , universeField  :: Field
  }

-- | Масса.
type Mass = Float

-- | Тело (например, звезда или планета).
data Body = Body
  { bodyPosition :: Point   -- ^ Положение.
  , bodyVelocity :: Vector  -- ^ Вектор скорости.
  , bodyMass     :: Mass    -- ^ Масса.
  }

-- | Несколько тел образуют систему тел с общей массой,
-- положением центра масс и скоростью центра масс.
instance Monoid Body where
  mempty = Body (0, 0) (0, 0) 0
  mappend (Body p1 v1 m1) (Body p2 v2 m2) = Body p v m
    where
      m = m1 + m2
      p = mulSV (1 / m) (mulSV m1 p1 + mulSV m2 p2)
      v = mulSV (1 / m) (mulSV m1 v1 + mulSV m2 v2)

-- | Векторное поле.
newtype Field = Field { getField :: Point -> Vector }

-- | Поле образует моноид с операцией сложения в каждой точке поля.
instance Monoid Field where
  mempty = Field (\_ -> (0, 0))
  mappend (Field f) (Field g) = Field (\p -> f p + g p)

-- | Поле ускорения свободного падения, порождённое одним телом.
bodyField :: Body -> Field
bodyField = Field . accel

-- | Вычислить вектор ускорения, которое получает объект
-- в заданной точке из-за взаимодействия с заданным телом.
accel :: Body -> Point -> Vector
accel body point
  | r == (0, 0) = (0, 0)  -- точка, в которой находится тело, выколота
  | otherwise   = mulSV (bigG * m / d^2) n
  where
    m = bodyMass body
    r = bodyPosition body - point
    n = normalizeV r
    d = magV r

-- | Вектор скорости, необходимый для круговой орбиты.
orbitVelocity :: Body -> Point -> Vector
orbitVelocity body point = bodyVelocity body + v
  where
    v = mulSV (sqrt (magV r * magV a)) (rotateV (pi/2) n)
    a = accel body point
    r = bodyPosition body - point
    n = normalizeV r

-- | Начальное состояние вселенной.
initUniverse :: [Body] -> Universe
initUniverse bodies = Universe
  { universeBodies = bodies
  , universeField  = mconcat (map bodyField bodies)
  }

-- | Отобразить вселенную.
drawUniverse :: Universe -> Picture
drawUniverse universe = mconcat
  [ mconcat (fmap drawBody (universeBodies universe)) ]

-- | Отобразить одно тело.
drawBody :: Body -> Picture
drawBody body = translate x y (color white (thickCircle (r/2) r))
  where
    (x, y) = bodyPosition body
    r = sqrt (bodyMass body)

-- | Обновить состояние вселенной.
updateUniverse :: ViewPort -> Float -> Universe -> Universe
updateUniverse _ dt universe = initUniverse newBodies
  where
    bodies = universeBodies universe
    field  = universeField universe
    newBodies = map (updateBody dt field) bodies

-- | Обновить тело, движущееся в заданном силовом поле.
updateBody :: Float -> Field -> Body -> Body
updateBody dt (Field f) body = body
  { bodyPosition = bodyPosition body + mulSV dt (bodyVelocity body)
  , bodyVelocity = bodyVelocity body + mulSV dt (f (bodyPosition body))
  }

-- | Гравитационная постоянная.
bigG :: Float
bigG = 500

-- | Ширина экрана.
screenWidth :: Num a => a
screenWidth = 500

-- | Высота экрана.
screenHeight :: Num a => a
screenHeight = 500
