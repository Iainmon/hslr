{-# LANGUAGE ExtendedDefaultRules #-}

module Test2 where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Art
import Control.Monad (when)
import Prelude hiding (cos,sin,length,abs,(.),(^))
import qualified Prelude (length,(.))
import RoseTree hiding (root)
import Run
import Data.Colour.SRGB

color = var Float "color"
st = var Vector2 "uv"
uv = st - vector (0.5,0.5)
time = var Float "u_time"
cos = cosine
sin = sine
norm = normalize

normOccilation = 0.5 * (sin time) + 0.5
colorf l = 0.5 + (0.5 * ( cos $ 3.1415 + (l * 0.15) + vector (0.1,0.66,1.0) ))
bp f r s = smoothstep r (r+s) f

yellow = vector (0.988,0.729,0.0117)
purple = vector (0.68, 0.23, 0.81)
white = vector (1,1,1)


toGLColor :: RGB Float -> SyntaxTree
toGLColor c = vector (channelRed c, channelGreen c, channelBlue c)
fromHex = toSRGB Prelude.. sRGB24read

glHexCol :: String -> SyntaxTree
glHexCol = toGLColor Prelude.. fromHex

pallet = map glHexCol [
        "276FBF"
        -- ,"F6F4F3"
        ,"183059"
        ,"FFC72E"
        ,"F03A47"
        -- ,"9BC53D"
        ]

pastellepallet = map glHexCol [
        "26547C"
        ,"EF476F"
        ,"FFD166"
        ,"06D6A0"
        ,"FCFCFC"
        ]
tropicalPallet = map glHexCol [
        "44AF69"
        ,"DBD5B5"
        ,"FCAB10"
        ,"2B9EB3"
        ,"DBD5B5"
        ,"F8333C"

    ]

cols = zip names $ pallet
        where names = ["blue", "ivory", "space", "red", "python"]

find _ [] = error ":("
find i (x:xs) = if fst x == i then snd x else find i xs

colorof name = find name cols

discColorf x = 0.1 * (App Vector3 "floor" [ 20.0 * colorf x])

-- fluidWarp st@(Vec2 x y) = App Vector2 "fluidWarp" [st]
fluidWarp st = App Vector2 "fluidWarp" [st]
-- select x t = clamp 0.0 1.0 ((1 - (x - t)) ^ 30.0)
-- select x t = ((-) 1 $ abs (t-x)) ^20
select x t ran = (smoothstep t (t+s) x) - (smoothstep (t+ran) ((t+ran)+s) x)
    where -- ran = 0.2
          s = 0.01


-- program = sum [(select b 0.5 0.2) * yellow, (select b 0.3 0.2) * red, (select b 0.7 0.3) * space, (select b 0.0 0.3) * ivory]
--     where z = (*) 2 $ (+) 0.5 $ (*) 0.5 $ perlin $ 6 *  fluidWarp (2 * uv)
--           c = discColorf z
--           green = colorof "python"
--           blue = colorof "blue"
--           red = colorof "red"
--           ivory = colorof "ivory"
--           space = colorof "space"
--           b = z * 0.5

nperlin :: SyntaxTree -> SyntaxTree
nperlin st = (+) 0.5 $ (*) 0.5 $ perlin st

-- program = discCols pastellepallet b
--     where z = nperlin $ normOccilation * 2 * fluidWarp (5 * uv)
--           c = discColorf z
--           b = z

-- program = discCols pastellepallet z
--     where z = nperlin $ (*) (50) $ (+) uv $ nperlin (uv * 10 * normOccilation)

program' = discCols pastellepallet z
    where z = nperlin $ (*) 50 $ (+) uv' $ nperlin (uv' * 20)
          uv' = (uv + vector (1,1)) * 0.08


program'' = discCols pastellepallet z
    where z = ((uv'.x ^2) + (uv'.y ^2)) / root 2
          uv' = (uv * 2) + (perlin (5*uv))

norma x = 0.5 + (0.5 * x)

-- program = discCols pastellepallet z
--     where z = (nperlin $ (+) uv $ nperlin uv')
--           uv' = (uv * 1) + (noise $ (+) uv $ (*) 10 $ noise (8*uv))

-- distField x y = 1/2*(x^12 + 6*x^10*y^2 + 15*x^8*y^4 + 20*x^6*y^6 + 15*x^4*y^8 + 6*x^2*y^10 + y^12 - 5*x^10 - 25*x^8*y^2 - 50*x^6*y^4 - 50*x^4*y^6 - 25*x^2*y^8 - 5*y^10 + 5*x^8 + 20*x^6*y^2 + 30*x^4*y^4 + 20*x^2*y^6 + 5*y^8 - 2*x^7 + 42*x^5*y^2 - 70*x^3*y^4 + 14*x*y^6)/root((6*x^11 + 30*x^9*y^2 + 60*x^7*y^4 + 60*x^5*y^6 + 30*x^3*y^8 + 6*x*y^10 - 25*x^9 - 100*x^7*y^2 - 150*x^5*y^4 - 100*x^3*y^6 - 25*x*y^8 + 20*x^7 + 60*x^5*y^2 + 60*x^3*y^4 + 20*x*y^6 - 7*x^6 + 105*x^4*y^2 - 105*x^2*y^4 + 7*y^6)^2 + (6*x^10*y + 30*x^8*y^3 + 60*x^6*y^5 + 60*x^4*y^7 + 30*x^2*y^9 + 6*y^11 - 25*x^8*y - 100*x^6*y^3 - 150*x^4*y^5 - 100*x^2*y^7 - 25*y^9 + 20*x^6*y + 60*x^4*y^3 + 60*x^2*y^5 + 20*y^7 + 42*x^5*y - 140*x^3*y^3 + 42*x*y^5)^2) -- (x^2*y + x*y^2)/root((x^2 + 2*x*y)^2 + (2*x*y + y^2)^2)
distField x y = (x^12 + 6*x^10*y^2 + 15*x^8*y^4 + 20*x^6*y^6 + 15*x^4*y^8 + 6*x^2*y^10 + y^12 - 5*x^10 - 25*x^8*y^2 - 50*x^6*y^4 - 50*x^4*y^6 - 25*x^2*y^8 - 5*y^10 + 5*x^8 + 20*x^6*y^2 + 30*x^4*y^4 + 20*x^2*y^6 + 5*y^8 - 2*x^7 + 42*x^5*y^2 - 70*x^3*y^4 + 14*x*y^6)/(root((x^12 + 6*x^10*y^2 + 15*x^8*y^4 + 20*x^6*y^6 + 15*x^4*y^8 + 6*x^2*y^10 - 5*x^10 - 25*x^8*y^2 - 50*x^6*y^4 - 50*x^4*y^6 - 25*x^2*y^8 + 5*x^8 + 20*x^6*y^2 + 30*x^4*y^4 + 20*x^2*y^6 - 2*x^7 + 42*x^5*y^2 - 70*x^3*y^4 + 14*x*y^6)^2 + (6*x^10*y^2 + 15*x^8*y^4 + 20*x^6*y^6 + 15*x^4*y^8 + 6*x^2*y^10 + y^12 - 25*x^8*y^2 - 50*x^6*y^4 - 50*x^4*y^6 - 25*x^2*y^8 - 5*y^10 + 20*x^6*y^2 + 30*x^4*y^4 + 20*x^2*y^6 + 5*y^8 + 42*x^5*y^2 - 70*x^3*y^4 + 14*x*y^6)^2) + 1)

    
-- program = discCols pastellepallet z
--     where z = clamp 0 1 (norma $ distField (uv'.x) (uv'.y))
--           uv' = (uv * 10) + (0.5 * fluidWarp (time + uv * 10))

circ = Vec2 (cos time) (sin time)

program = discCols pastellepallet z
    where z = (normOccilation) - fbm (uv * 10) -- (5 * normOccilation) + distField x' y'
          uv' = uv * 4
          x' = uv'.x
          y' = uv'.y

discCols :: [SyntaxTree] -> SyntaxTree -> SyntaxTree
discCols cols z = sum $ map f pairs
    where n = (fromIntegral $ Prelude.length cols) :: Float
          pairs = (zip [0.0..n] cols)  :: [(Float, SyntaxTree)]
          f (m,x) = x * select z (syntaxNode (m/n)) (syntaxNode ((1.0/n)))

testProg = discCols pallet $ nperlin $ fluidWarp (5 * uv)


main = do
    let newContents = generateProgram $ flattenAssociativeOperations $ optimize program
    let shaderFile = "out.frag"
    save newContents shaderFile
    run $ Params { width = 720, height = 720, fileName = shaderFile}
m = main
