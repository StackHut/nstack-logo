module Main where
import Data.Maybe
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

nHorizLerp :: Double
nHorizLerp = 0.52

nDiagHeight :: Point V2 Double
nDiagHeight = 1.45

main :: IO ()
main = mainWith $ (hex <> fromJust (frontFace hex) <> fromJust (nNeg hex)) # scaleX 0.9 # lw veryThick

hexNames :: [[String]]
hexNames = [map (("hex" ++) . show) [0..]]

hex :: Diagram B
hex = stroke' (with & vertexNames .~ hexNames) hexTrail # centerXY

hexTrail :: Trail V2 Double
hexTrail = regPoly 6 2 # rotateBy (1/4)

frontFace :: Diagram B -> Maybe (Diagram B)
frontFace d = do a <- location <$> lookupName "hex4" d
                 b <- location <$> lookupName "hex5" d
                 c <- location <$> lookupName "hex0" d
                 let d = c ^-^ b
                     e = d * 0.25
                     f = strokeLoop . closeLine $ fromVertices [a, b, b ^+^ e, a ^+^ e]
                     g = mconcat [moveTo a f, moveTo (a + e) f, moveTo (a + e * 2) f, moveTo (a + e * 3) f]
                     h = moveTo a . strokeLoop . closeLine $ fromVertices [a, b, c, a ^+^ d]
                 return $ g <> h # fc white

nNeg :: Diagram B -> Maybe (Diagram B)
nNeg dia = do a <- location <$> lookupName "hex0" dia
              b <- location <$> lookupName "hex1" dia
              c <- location <$> lookupName "hex2" dia
              d <- location <$> lookupName "hex5" dia
              e <- location <$> lookupName "hex4" dia
              f <- location <$> lookupName "hex3" dia
              let t1 = lerp nHorizLerp b a
                  t2 = lerp nHorizLerp b c
                  b1 = lerp nHorizLerp e d
                  b2 = lerp nHorizLerp e f
                  b4 = t2 - (unitY * nDiagHeight)
                  t4 = b1 + (unitY * nDiagHeight)
                  d1 = moveTo t1 . strokeLoop . closeLine $ fromVertices [t1, b, t2, t4]
                  d2 = moveTo b1 . strokeLoop . closeLine $ fromVertices [b1, e, b2, b4]
                  --dx = moveTo f . strokeLoop . closeLine $ fromVertices [f, e, e ^+^ (a ^-^ d), f ^+^ (a ^-^ d)]
              --return $ d1 # fc grey # lw none <> d2 # fc black # lw none <> dx # fc lightgrey # lw none
              return $ d1 # fc black # lw none <> d2 # fc black # lw none
