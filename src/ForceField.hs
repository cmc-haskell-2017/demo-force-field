module ForceField where

import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Vector

-- | Запустить демонстрацию.
demo :: [Body] -> IO ()
demo bodies = simulate display bgColor fps (initUniverse bodies) drawUniverse updateUniverse
  where
    display = InWindow "Flappy Lambda" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 300     -- кол-во кадров в секунду

-- | Солнечная система c планетами.
sampleStarSystem :: [Body]
sampleStarSystem = [ sun, venus, earth, moon ]
  where
    sun = Body (-100, 0) (-5, 5) 100000

    venus   = orbitingAt (0, 50)  sun 10000
    earth   = orbitingAt (370, -50) sun 30000
    moon    = orbitingAt (0, 25) earth 500

    orbitingAt pos body mass = Body point (orbitVelocity body point) mass
      where
        point = bodyPosition body + pos

-- | Модель вселенной.
data Universe = Universe
  { universeBodies :: [Body]  -- ^ Тела во вселенной.
  , universeField  :: Field   -- ^ Силовое поле, построенное по телам.
  }

-- | Масса.
type Mass = Float

-- | Тело (например, звезда или планета).
data Body = Body
  { bodyPosition :: Point   -- ^ Положение.
  , bodyVelocity :: Vector  -- ^ Вектор скорости.
  , bodyMass     :: Mass    -- ^ Масса.
  }

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
  [ drawField 40 (universeField universe)
  , mconcat (fmap drawBody (universeBodies universe))
  ]

-- | Отобразить одно тело.
drawBody :: Body -> Picture
drawBody body = translate x y (color white (thickCircle (r/2) r))
  where
    (x, y) = bodyPosition body
    r = bodyMass body ** 0.3

drawField :: Float -> Field -> Picture
drawField d field = mconcat (map (drawFieldAtPoint d field) points)
  where
    points =
      [ (x - w/2, y - h/2)
      | x <- [0, d .. w]
      , y <- [0, d .. h]
      ]
    w = screenWidth
    h = screenHeight

drawFieldAtPoint :: Float -> Field -> Point -> Picture
drawFieldAtPoint d (Field f) (x, y) = color c (translate x y (scale s s (rotate theta arrow)))
  where
    c = makeColor m 0 (1 - m) 1
    s = 0.7 * d * m
    v = f (x, y)
    theta = angle (1, 0) v * 180/pi
    m = 1 - exp (log 0.5 * magV v / mediumAccel)

angle :: Vector -> Vector -> Float
angle v1 v2 = - atan2 y x
  where
    (x, y) = v2 - v1

arrow :: Picture
arrow = mconcat
  [ polygon [ (0, w), (1 - hl, w), (1 - hl, -w), (0, -w) ]
  , polygon [ (1 - hl, hw), (1, 0), (1 - hl, -hw) ]
  ]
  where
    w  = 0.05 -- полширины стрелки
    hw = 0.2  -- полширины головы стрелки
    hl = 0.4  -- длина головы стрелки

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
bigG = 4

-- | Абсолютное значение ускорения, которое считается средним.
-- Это значение используется для калиброки визуализации.
mediumAccel :: Float
mediumAccel = magV (accel (Body (0, 0) (0, 0) 20000) (100, 0))

-- | Ширина экрана.
screenWidth :: Num a => a
screenWidth = 800

-- | Высота экрана.
screenHeight :: Num a => a
screenHeight = 800
