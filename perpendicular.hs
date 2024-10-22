-- Función para calcular la pendiente de un segmento de línea
pendiente :: (Double, Double) -> (Double, Double) -> Maybe Double
pendiente (x1, y1) (x2, y2)
  | x2 - x1 == 0 = Nothing   -- Línea vertical, pendiente indefinida
  | otherwise    = Just ((y2 - y1) / (x2 - x1))

-- Función para verificar si dos segmentos de línea son perpendiculares
sonPerpendiculares :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
sonPerpendiculares p1 p2 p3 p4 =
  case (pendiente p1 p2, pendiente p3 p4) of
    (Nothing, Just 0) -> True  -- Primer segmento es vertical, el segundo es horizontal
    (Just 0, Nothing) -> True  -- Primer segmento es horizontal, el segundo es vertical
    (Just m1, Just m2) -> m1 * m2 == -1  -- Verifica si el producto de las pendientes es -1
    _ -> False  -- Otros casos no son perpendiculares

-- Ejemplo de uso: dos segmentos de línea perpendiculares
main :: IO ()
main = do
  let p1 = (0, 0)
      p2 = (1, 3)
      p3 = (0, 1)
      p4 = (2, 0)
  
  if sonPerpendiculares p1 p2 p3 p4
    then putStrLn "Los segmentos de línea son perpendiculares."
    else putStrLn "Los segmentos de línea no son perpendiculares."