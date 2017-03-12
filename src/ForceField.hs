module ForceField where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

-- | Запустить демонстрацию.
demo :: [Body] -> IO ()
demo bodies = play display bgColor fps (initUniverse bodies) drawUniverse handleUniverse updateUniverse
  where
    display = InWindow "Визуализация гравитационного поля" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 300     -- кол-во кадров в секунду

-- | Солнечная система c планетами.
sampleStarSystem :: [Body]
sampleStarSystem = [ sun, venus, earth, moon ]
  where
    sun = Body (-100, 0) (0, 10) 200000

    venus   = orbitingAt (-50, 120)  sun 5000
    earth   = orbitingAt (370, -50) sun 50000
    moon    = orbitingAt (0, 50) earth 500

    orbitingAt pos body mass = Body point (orbitVelocity body point) mass
      where
        point = bodyPosition body + pos

-- ==============================================
-- Модель
-- ==============================================

-- | Модель вселенной.
data Universe = Universe
  { universeBodies    :: [Body]   -- ^ Тела во вселенной.
  , universeField     :: Field    -- ^ Силовое поле, построенное по телам.
  , universeBounds    :: Bounds   -- ^ Границы моделирования.
  , universeArrowSize :: Float    -- ^ Размер стрелок векторов.
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

-- | Границы моделируемой области.
data Bounds
  = Unbounded                 -- ^ Неограниченное.
  | Rectangular Point Point   -- ^ Прямоугольная область.

-- | Определить границы области, захватывающей все тела.
bounds :: [Body] -> Bounds
bounds _ = Unbounded  -- реализуйте эту функцию самостоятельно

-- | Начальное состояние вселенной.
initUniverse :: [Body] -> Universe
initUniverse bodies = Universe
  { universeBodies = bodies
  , universeField  = systemField bodies
  , universeBounds = toScreenProps (bounds bodies)
  , universeArrowSize = 0
  }

-- ==============================================
-- Физика
-- ==============================================

-- | Поле ускорения свободного падения, порождённое одним телом.
bodyField :: Body -> Field
bodyField = Field . accel

-- | Поле ускорения свободного падения, порождённое системой тел.
systemField :: [Body] -> Field
systemField = mconcat . map bodyField

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

-- ==============================================
-- Отрисовка
-- ==============================================

-- | Отобразить вселенную.
drawUniverse :: Universe -> Picture
drawUniverse universe = scaleToBounds (universeBounds universe) (mconcat
  [ drawField 30 (universeBounds universe) (universeArrowSize universe) (universeField universe)
  , mconcat (fmap drawBody (universeBodies universe))
  ])

-- | Отобразить одно тело.
drawBody :: Body -> Picture
drawBody body = translate x y (color white (thickCircle (r/2) r))
  where
    (x, y) = bodyPosition body
    r = bodyMass body ** 0.3

-- | Отобразить силовое поле.
drawField :: Float -> Bounds -> Float -> Field -> Picture
drawField cellSize boundary arrowSize field = mconcat (map (drawFieldAtPoint (cellSize * arrowSize) field) points)
  where
    points =
      [ (x, y)
      | x <- [l, l + cellSize .. r]
      , y <- [b, b + cellSize .. t]
      ]
    ((l, b), (r, t)) = case boundary of
      Unbounded -> ((-screenWidth/2, -screenHeight/2), (screenWidth/2, screenHeight/2))
      Rectangular lb rt -> (lb, rt)

-- | Отобразить один вектор силового поля.
drawFieldAtPoint :: Float -> Field -> Point -> Picture
drawFieldAtPoint arrowSize (Field f) (x, y) = color c (translate x y (scale s s (rotate theta arrow)))
  where
    c = makeColor m 0 (1 - m) 1
    s = arrowSize * m
    v = f (x, y)
    theta = angle (1, 0) v * 180/pi
    m = 1 - exp (log 0.5 * magV v / mediumAccel)

-- | Угол между двумя векторами.
angle :: Vector -> Vector -> Float
angle v1 v2 = - atan2 y x
  where
    (x, y) = v2 - v1

-- | Отобразить единичную стрелку.
arrow :: Picture
arrow = mconcat
  [ polygon [ (0, w), (1 - hl, w), (1 - hl, -w), (0, -w) ]  -- стержень
  , polygon [ (1 - hl, hw), (1, 0), (1 - hl, -hw) ]         -- голова
  ]
  where
    w  = 0.05 -- полширины стрелки
    hw = 0.2  -- полширины головы стрелки
    hl = 0.4  -- длина головы стрелки

-- | Привести пропорции границ моделирования к пропорциям экрана.
-- Новая граница будет больше либо равна исходной по каждому измерению.
toScreenProps :: Bounds -> Bounds
toScreenProps Unbounded = Unbounded
toScreenProps (Rectangular (l, b) (r, t))
  | tooWide   = Rectangular (l, b') (r, t')
  | otherwise = Rectangular (l', b) (r', t)
  where
    tooWide = (r - l) * screenHeight > (t - b) * screenWidth
    l' = (r + l - (t - b) * screenWidth / screenHeight) / 2
    r' = (r + l + (t - b) * screenWidth / screenHeight) / 2
    b' = (t + b - (r - l) * screenHeight / screenWidth) / 2
    t' = (t + b + (r - l) * screenHeight / screenWidth) / 2

-- | Масштабировать изображение таким образом,
-- чтобы заданная область занимала весь экран.
scaleToBounds :: Bounds -> Picture -> Picture
scaleToBounds Unbounded = id
scaleToBounds (Rectangular (l, b) (r, t))
  = scale sx sy . translate dx dy
  where
    dx = - (l + r) / 2
    dy = - (b + t) / 2
    sx = screenWidth / (r - l)
    sy = screenHeight / (t - b)

-- ==============================================
-- Обработка событий и обновление
-- ==============================================

-- | Обработка событий.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = toggleField
handleUniverse _ = id

-- | Переключить отображение силового поля.
toggleField :: Universe -> Universe
toggleField universe = universe { universeArrowSize = newArrowSize }
  where
    newArrowSize
      | universeArrowSize universe == 0 = 0.01
      | otherwise = 0

-- | Обновить состояние вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt universe = (initUniverse newBodies)
  { universeArrowSize = newArrowSize }
  where
    newBodies = map (updateBody dt field) bodies
    newArrowSize
      | arrowSize == 0 = 0
      | otherwise      = min 0.7 (arrowSize + dt * 0.8)

    bodies    = universeBodies universe
    field     = universeField universe
    arrowSize = universeArrowSize universe

-- | Обновить тело, движущееся в заданном силовом поле.
updateBody :: Float -> Field -> Body -> Body
updateBody dt (Field f) body = body
  { bodyPosition = bodyPosition body + mulSV dt (bodyVelocity body)
  , bodyVelocity = bodyVelocity body + mulSV dt (f (bodyPosition body))
  }

-- ==============================================
-- Константы и параметры моделирования
-- ==============================================

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
screenHeight = 600
