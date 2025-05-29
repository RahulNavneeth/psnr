{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.Vector as VU
import Data.Maybe
import Data.Word (Word8)
import Control.Exception (try, IOException)
import qualified Codec.Picture as JP
import System.IO (readFile)

type Codec = String
type RGB = JP.PixelRGB8
type Image = (JP.Image RGB)

sampleImgPath :: FilePath
sampleImgPath = "assets/lennahq.png"

sampleImgCmpPath :: FilePath
sampleImgCmpPath = "assets/lennahq-cmp.png"

supportedCodec :: VU.Vector Codec
supportedCodec = VU.fromList ["jpg", "jpeg", "png"]

getCodec :: FilePath -> Codec
getCodec path = drop 1 $ snd $ break ((==)'.') path

validatePath :: FilePath -> Bool
validatePath path = (not . null) codec &&  elem codec supportedCodec
	where
		codec = getCodec path

validateDimension :: Image -> Image -> Bool
validateDimension x y =
  JP.imageWidth x == JP.imageWidth y && JP.imageHeight x == JP.imageHeight y


-- validateCodec :: Codec -> Codec -> Bool
-- validateCodec c1 c2 = all $ map (elem supportedCodec . getCodec) [c1, c2]

getImageData :: FilePath -> IO (Maybe Image)
getImageData path
  | validatePath path = do
  		result <- JP.readImage path :: IO (Either String JP.DynamicImage)
		case result of
			Right img -> return $ Just (JP.convertRGB8 img)
			Left  err -> do
				putStrLn $ "Err: " ++ show err
				return Nothing

  | otherwise 	      = return Nothing
  
instance Num RGB where
  (JP.PixelRGB8 r1 g1 b1) + (JP.PixelRGB8 r2 g2 b2) = JP.PixelRGB8 (r1 + r2) (g1 + g2) (b1 + b2)
  (JP.PixelRGB8 r1 g1 b1) - (JP.PixelRGB8 r2 g2 b2) = JP.PixelRGB8 (r1 - r2) (g1 - g2) (b1 - b2)
  (JP.PixelRGB8 r1 g1 b1) * (JP.PixelRGB8 r2 g2 b2) = JP.PixelRGB8 (r1 * r2) (g1 * g2) (b1 * b2)

  abs (JP.PixelRGB8 r g b) = JP.PixelRGB8 (abs r) (abs g) (abs b)
  signum _ = JP.PixelRGB8 1 1 1
  fromInteger i = let x = fromInteger i :: Word8 in JP.PixelRGB8 x x x

meanSquareError :: Image -> Image -> RGB 
meanSquareError image1 image2 = summedMeanValue
  where 
    meanValue = VU.generate (JP.imageWidth image1 * JP.imageHeight image1) $ \i ->
      let x = i `mod` JP.imageWidth image1
          y = i `div` JP.imageWidth image1
          p1 = JP.pixelAt image1 x y
          p2 = JP.pixelAt image2 x y
      in (p1 - p2) ^ 2

    summedMeanValue = VU.foldl' (+) (JP.PixelRGB8 0 0 0) meanValue

instance Show Image where
	show img = "Image (" ++ show (JP.imageWidth img) ++ " X " ++ show (JP.imageHeight img) ++ ")"

main :: IO (Either String RGB) 
main 
	| allEq codecArr = do
		image1 <- getImageData sampleImgPath
		image2 <- getImageData sampleImgCmpPath
		case (image1, image2) of
			(Just x, Just y)
					| validateDimension x y -> do
							putStrLn $ (show x) ++ "\n" ++ (show y)
							return $ Right (meanSquareError x y)
					| otherwise -> return $ Left ":( Invalid dimensions"
			(Nothing, _) 	 -> return $ Left ":( Image 1"
			(_, Nothing) 	 -> return $ Left ":( Image 2"

	| otherwise = return $ Left ":( Invalid codec"
	where
		codecArr = map getCodec [sampleImgPath, sampleImgCmpPath]
		allEq (a:ab) = all (==a) ab
		allEq [] = True
		
