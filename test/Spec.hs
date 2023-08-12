{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TupleSections,
    OverloadedStrings #-}
{-# Rank2Types #-}

import Control.Monad (foldM)
import System.Exit (exitFailure)
import Data.DataPack.RoundTripSpec (packUnpackTests)
import qualified Data.DataPack.UnpackSpec as U
import qualified Data.DataPack.PackSpec as P
import qualified Data.ByteString.Lazy as C
import Data.DataPack.Unpack (PackType, UnpackError, UnpackState)
import Data.DataPack.Pack (PackError)
import Data.Int (Int64)

runTests :: (Foldable t, Show a) => t ([Char], IO (Maybe a)) -> IO ()
runTests testCases = do
  putStrLn "Running tests...\n"
  (runCount, failCount) <- foldM (\ (runC, failC) (testName, resultsM) -> do
    results <- resultsM
    let runCount' = runC + 1
    putStrLn $ '\t':show runCount' ++ ") " ++ show results ++ " "
      ++ testName
    pure (runCount', failC + (
      case results of
        Nothing -> 0
        _ -> 1))
    ) (0, 0) testCases
  if failCount == 0
    then putStrLn $ "\nSuccess! All " ++ show runCount ++ " test(s) passed."
    else putStrLn ('\n':show failCount ++ " out of " ++ show runCount
        ++ " test(s) failed.") >> exitFailure

testString :: String
testString = "Cake tootsie roll chocolate bar dragée cupcake carrot cake cotton\
  \ candy sesame snaps. Cake sweet roll oat cake I love cake oat cake liquorice\
  \ toffee halvah. Tootsie roll I love jelly fruitcake bear claw candy canes. I\
  \ love apple pie liquorice. Lemon drops cotton candy jelly tootsie roll jelly\
  \ caramels candy. Gingerbread I love sugar plum bonbon marshmallow I love \
  \jelly beans topping pastry. I love candy canes I love oat cake muffin candy \
  \canes cotton candy I love. Caramels pudding pastry pie sweet sweet I love \
  \pastry. Oat cake lollipop cake I love soufflé jelly beans. Tiramisu toffee \
  \oat cake icing pie chocolate bar apple pie. Dragée cookie caramels pastry \
  \wafer. Gummies I love jelly jelly beans. I love dragée jelly jelly beans. \
  \Topping ice cream jelly-o biscuit.\n\nSugar plum macaroon liquorice \
  \chocolate cake chocolate. Lemon drops powder cupcake. Marzipan pastry ice \
  \cream tiramisu gummies apple pie cheesecake chocolate bar lemon drops. \
  \Croissant caramels sweet roll jujubes dragée carrot cake. Jujubes jujubes \
  \candy ice cream I love. Tootsie roll liquorice cheesecake I love I love \
  \cotton candy sweet lollipop. Tootsie roll carrot cake pudding cake I love I \
  \love. Biscuit I love dessert gummi bears tootsie roll tart jujubes. Jelly \
  \marzipan I love marzipan candy oat cake biscuit. Powder wafer oat cake I \
  \love chocolate bear claw pie croissant. Candy candy soufflé powder biscuit. \
  \Topping I love I love sesame snaps.\n\nOat cake chocolate cake tiramisu \
  \danish cake. Pudding gummies marzipan. Gingerbread brownie tiramisu carrot \
  \cake ice cream I love donut danish. Icing toffee dessert croissant candy \
  \canes candy jujubes. Cake I love tiramisu tart jelly beans. Soufflé bonbon \
  \dessert I love icing. Powder chocolate I love apple pie. Croissant apple pie\
  \ dragée dragée chocolate bar candy canes jujubes oat cake. Dragée \
  \gingerbread chocolate cake lemon drops gummies biscuit. Marzipan chocolate \
  \cake bear claw. Carrot cake pudding jujubes icing sugar plum I love chupa \
  \chups. Ice cream ice cream I love.\n\nJelly beans cupcake dessert bear claw \
  \candy canes chupa chups gummi bears. Chocolate bar lemon drops toffee candy \
  \canes gingerbread. Pie toffee chocolate I love powder halvah apple pie. \
  \Gummies cheesecake apple pie. Marzipan pudding liquorice icing. Tootsie roll\
  \ cake I love jujubes. I love dessert oat cake sesame snaps biscuit. I love \
  \chupa chups jelly-o caramels cotton candy marzipan wafer. Powder gummies \
  \donut bear claw icing jelly beans dessert chocolate bar. Brownie jelly \
  \toffee pastry cake. Donut biscuit chocolate liquorice I love candy croissant\
  \ carrot cake caramels. Toffee chupa chups dessert chupa chups chocolate cake\
  \ macaroon. Cheesecake chupa chups dessert pie. Sesame snaps I love bear claw\
  \.\n\nCaramels oat cake tart cake biscuit I love cupcake. Croissant lemon \
  \drops I love I love. Jelly-o sweet roll oat cake I love. Chocolate ice cream\
  \ tart. Chocolate bar gummies sweet roll I love marshmallow danish chupa \
  \chups bear claw. Carrot cake I love cheesecake gummi bears bear claw \
  \cheesecake brownie. Sugar plum cake chupa chups chocolate bar. Lollipop \
  \sweet roll toffee lemon drops cotton candy candy canes chocolate cake \
  \chocolate cake jelly beans. I love chocolate I love. Jujubes jelly beans pie\
  \ candy. Jelly-o jelly-o chocolate cake soufflé macaroon I love dragée \
  \pudding cake. Lemon drops brownie jelly powder pudding. Jelly jelly-o apple\
  \ pie. Dragée candy lemon drops gummies sugar plum pastry donut lemon drops\
  \ croissant."

main :: IO ()
main = do
  -- pack the test string
  -- unpack it
  -- compare it to the original

  runTests (U.unpackTests :: [(String, IO (Maybe
          ((Either (UnpackError () Int64) PackType,
            (UnpackState Int64, C.ByteString, C.ByteString)),
          (Either (UnpackError () Int64) PackType,
            (UnpackState Int64, C.ByteString, C.ByteString)))))])
  runTests P.packTests
  runTests (packUnpackTests :: [(String, IO (Maybe
          (Either
              (PackError (), C.ByteString)
              ((Either (UnpackError () Int64) PackType,
                (UnpackState Int64, C.ByteString, C.ByteString)),
              (Either (UnpackError () Int64) PackType,
                (UnpackState Int64, C.ByteString, C.ByteString))))))])
