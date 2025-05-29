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
type MSE = Float
type PSNR = Float

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
  
meanSquareError :: Image -> Image -> MSE 
meanSquareError image1 image2 = constantValue * fromIntegral summedMeanValue
  where 
    meanValue = VU.generate (JP.imageWidth image1 * JP.imageHeight image1) $ \i ->
      let x = i `mod` JP.imageWidth image1
          y = i `div` JP.imageWidth image1
          JP.PixelRGB8 r1 g1 b1 = JP.pixelAt image1 x y
          JP.PixelRGB8 r2 g2 b2 = JP.pixelAt image2 x y
          diff a b = (fromIntegral a - fromIntegral b) ^ 2
      in diff (r1) (r2) + diff (g1) (g2) + diff (b1) (b2)

    constantValue = 1.0/(3.0*fromIntegral(JP.imageWidth image1 * JP.imageHeight image1))
    summedMeanValue = VU.foldl' (+) 0 meanValue

instance Show Image where
	show img = "Image (" ++ show (JP.imageWidth img) ++ " X " ++ show (JP.imageHeight img) ++ ")"

psnr :: MSE -> PSNR
psnr mse = (20 * logBase 10 255)  - (10 * logBase 10 mse)

main :: IO (Either String PSNR) 
main 
	| allEq codecArr = do
		image1 <- getImageData sampleImgPath
		image2 <- getImageData sampleImgCmpPath
		case (image1, image2) of
			(Just x, Just y)
					| validateDimension x y -> do
							putStrLn $ (show x) ++ "\n" ++ (show y)
							return $ Right (psnr $ meanSquareError x y)
					| otherwise -> return $ Left ":( Invalid dimensions"
			(Nothing, _) 	 -> return $ Left ":( Image 1"
			(_, Nothing) 	 -> return $ Left ":( Image 2"

	| otherwise = return $ Left ":( Invalid codec"
	where
		codecArr = map getCodec [sampleImgPath, sampleImgCmpPath]
		allEq (a:ab) = all (==a) ab
		allEq [] = True
		
