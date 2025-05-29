{-# LANGUAGE EmptyCase #-}
module Main where

import qualified Data.Vector as VU
import Data.Maybe
import Control.Exception (try, IOException)
import qualified Data.ByteString as BS

newtype RGB = RGB { rgbToVector :: VU.Vector Int } deriving Show
newtype Image = Image { imageToVector :: VU.Vector RGB }

instance Show Image where
	show (Image x) = "Image:"++show x

type Codec = String

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

-- validateCodec :: Codec -> Codec -> Bool
-- validateCodec c1 c2 = all $ map (elem supportedCodec . getCodec) [c1, c2]

getImageData :: FilePath -> IO (Maybe Image)
getImageData path
  | validatePath path = do
  		result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
		case result of
			Right _ -> return (Just $ Image VU.empty)
			Left  _ -> return Nothing

  | otherwise 	      = return Nothing

main :: IO () 
main 
	| all (== head codecArr) codecArr = do
		image1 <- getImageData sampleImgPath
		image2 <- getImageData sampleImgCmpPath
		case (image1, image2) of
			(Just x, Just y) ->
				putStrLn $ "RES: " ++ show x ++ " " ++ show y
			(Nothing, _) 	 -> print ":( Image 1"
			(_, Nothing) 	 -> print ":( Image 2"

	| otherwise = print ":( Invalid codec"
	where
		codecArr = map getCodec [sampleImgPath, sampleImgCmpPath]
