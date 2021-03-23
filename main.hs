{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Control.Monad (when)
import Prelude hiding (cos,sin,length,abs,(.),(^))
import qualified Prelude (length)
import Art
color = var Float "color"
st = var Vector2 "uv"
uv = st - vector (0.5,0.5)
time = var Float "u_time"
cos = cosine
sin = sine
norm = normalize
barCount = 100.0 * (sin time)
program = vector (c * 0.6 * (sine $ barCount * uv.y + time), (norm $ (cos uv.x) + (sin uv.y))/2, c * (root time))
        where col = (c * 0.6, c * 0.3, c * 0.5)
              -- c = cos $ time * 0.4 + (uv.x * 30)
              c = uv * uv * uv * uv + uv + uv + uv + uv

normOccilation = 0.5 * (sin time) + 0.5


colorf l = 0.5 + (0.5 * ( cos $ 3.1415 + (l * 0.15) + vector (0.0,0.6,1.0) ))

biasPlane = smoothstep r (r+s) z
    where z = root $ (uv.x * uv.>x) + (uv.y * uv\.y)
          r = 0.2 + (normOccilation * 0.2) + (0.2 * perlin (uv + time)) + (0.2 * perlin (uv + time + 10.0))
          s = 0.02

bp f r s = smoothstep r (r+s) f

-- dfield x y = x^10 - 5/2*x^8 + 35/16*x^6 - 25/32*x^4 + 25/256*x^2 + 1/16*y^6 - 3/32*y^4 + 9/256*y^2 - 1/256
-- dfield x y = x^24 + 12*x^22*y^2 - 11*x^22 + 66*x^20*y^4 - 121*x^20*y^2 + 44*x^20 + 220*x^18*y^6 - 605*x^18*y^4 + 440*x^18*y^2 - 77*x^18 + 495*x^16*y^8 - 1815*x^16*y^6 + 1980*x^16*y^4 - 693*x^16*y^2 + 55*x^16 + 792*x^14*y^10 - 3630*x^14*y^8 + 5280*x^14*y^6 - 2772*x^14*y^4 + 440*x^14*y^2 - 11*x^14 - 2*x^13 + 924*x^12*y^12 - 5082*x^12*y^10 + 9240*x^12*y^8 - 6468*x^12*y^6 + 1540*x^12*y^4 - 77*x^12*y^2 + 156*x^11*y^2 + 792*x^10*y^14 - 5082*x^10*y^12 + 11088*x^10*y^10 - 9702*x^10*y^8 + 3080*x^10*y^6 - 231*x^10*y^4 - 1430*x^9*y^4 + 495*x^8*y^16 - 3630*x^8*y^14 + 9240*x^8*y^12 - 9702*x^8*y^10 + 3850*x^8*y^8 - 385*x^8*y^6 + 3432*x^7*y^6 + 220*x^6*y^18 - 1815*x^6*y^16 + 5280*x^6*y^14 - 6468*x^6*y^12 + 3080*x^6*y^10 - 385*x^6*y^8 - 2574*x^5*y^8 + 66*x^4*y^20 - 605*x^4*y^18 + 1980*x^4*y^16 - 2772*x^4*y^14 + 1540*x^4*y^12 - 231*x^4*y^10 + 572*x^3*y^10 + 12*x^2*y^22 - 121*x^2*y^20 + 440*x^2*y^18 - 693*x^2*y^16 + 440*x^2*y^14 - 77*x^2*y^12 - 26*x*y^12 + y^24 - 11*y^22 + 44*y^20 - 77*y^18 + 55*y^16 - 11*y^14
dfield x y = 10*x^24 + 120*x^22*y^2 + 660*x^20*y^4 + 2200*x^18*y^6 + 4950*x^16*y^8 + 7920*x^14*y^10 + 9240*x^12*y^12 + 7920*x^10*y^14 + 4950*x^8*y^16 + 2200*x^6*y^18 + 660*x^4*y^20 + 120*x^2*y^22 + 10*y^24 - 110*x^22 - 1210*x^20*y^2 - 6050*x^18*y^4 - 18150*x^16*y^6 - 36300*x^14*y^8 - 50820*x^12*y^10 - 50820*x^10*y^12 - 36300*x^8*y^14 - 18150*x^6*y^16 - 6050*x^4*y^18 - 1210*x^2*y^20 - 110*y^22 + 440*x^20 + 4400*x^18*y^2 + 19800*x^16*y^4 + 52800*x^14*y^6 + 92400*x^12*y^8 + 110880*x^10*y^10 + 92400*x^8*y^12 + 52800*x^6*y^14 + 19800*x^4*y^16 + 4400*x^2*y^18 + 440*y^20 - 770*x^18 - 6930*x^16*y^2 - 27720*x^14*y^4 - 64680*x^12*y^6 - 97020*x^10*y^8 - 97020*x^8*y^10 - 64680*x^6*y^12 - 27720*x^4*y^14 - 6930*x^2*y^16 - 770*y^18 + 550*x^16 + 4400*x^14*y^2 + 15400*x^12*y^4 + 30800*x^10*y^6 + 38500*x^8*y^8 + 30800*x^6*y^10 + 15400*x^4*y^12 + 4400*x^2*y^14 + 550*y^16 - 110*x^14 - 770*x^12*y^2 - 2310*x^10*y^4 - 3850*x^8*y^6 - 3850*x^6*y^8 - 2310*x^4*y^10 - 770*x^2*y^12 - 110*y^14 - 20*x^13 + 1560*x^11*y^2 - 14300*x^9*y^4 + 34320*x^7*y^6 - 25740*x^5*y^8 + 5720*x^3*y^10 - 260*x*y^12

yellow = vector (0.988,0.729,0.0117)
purple = vector (0.68, 0.23, 0.81)
white = vector (1,1,1)

program2 = mix col white biasPlane
    where col = yellow
          white = colorf (time * 5)


program3 = mix col white biasPlane
    where col = yellow
          biasPlane = bp (perlin $ st * 10 + (perlin (20 * uv + time))) (normOccilation) 0.001


program4 = compileLayers layers
    where biasPlane = bp (perlin $ st * 10 + (perlin (20 * uv + time))) 0.01 0.001
          layers = Combination (Color purple) (Combination (Color white) (Color $ colorf (time*2.0)) biasPlane) $ circle 0.1

program5 = mix white (colorf $ 10 * (perlin $ (uv * 8) + time) + time * 2) $ clamp 0.0 1.0 z
    where z = 10000 * (dfield (uv'.x) (uv'.y)) -- bp (0.0001 * (abs $ sin time)) 0.0 $ abs $ dfield (uv'.x) (uv'.y)
          uv' = uv * 3 -- (uv + 0.2 *(perlin (uv * 2 + time))) * 3

main = do
    let newContents = generateProgram  $ optimize $ flattenCommutitiveOperations program5
    when (Prelude.length newContents > 0) $
        writeFile "out.frag" newContents
m = main