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

program = discCols pastellepallet z
    where z = nperlin $ (*) 50 $ (+) uv' $ nperlin (uv' * 20)
          uv' = (uv + vector (1,1)) * 0.08


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
    run $ Params { width = 720, height = 900, fileName = shaderFile}
m = main
