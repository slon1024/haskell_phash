import Data.PHash
import Control.Monad (sequence)

pairs :: (Ord a) => [a] -> [(a, a)]
pairs list = [ (a, b) | a <- list, b <- list, a <= b, a /= b ]

compareHash :: Maybe PHash -> Maybe PHash -> String
compareHash (Just h1) (Just h2) =  show $ hammingDistance h1 h2
compareHash _ _                 = "Error, could not read images"


readImg :: (FilePath, FilePath) -> IO (String,String, String)
readImg (x,y) = do
    let prefix = "img/"
    hash_one <- imageHash $ prefix ++ x
    hash_two <- imageHash $ prefix ++ y
    let diff =  compareHash hash_one hash_two
    return (x,y, diff)


main = do
    let images = ["cat00.jpg", "cat01.jpg", "cat02.jpg"]
--     let images = ["cat10.jpg", "cat11.jpg", "cat12.jpg"]
--     let images = ["dog20.jpg", "dog21.jpg", "dog22.jpg"]
--     let images = ["dog20.jpg", "cat00.jpg", "cat10.jpg"]
--     let images = ["dog20.jpg", "dog_crop0.jpg", "dog_crop1.jpg"]
--     let images = ["dog20.jpg", "dog_img0.jpg", "dog_img1.jpg", "dog_img2.jpg"]
--     let images = ["black.jpg",  "blue.jpg", "green.jpg", "orange.jpg", "white.jpg", "yellow.jpg"]


    sequence $ map readImg $ pairs images
