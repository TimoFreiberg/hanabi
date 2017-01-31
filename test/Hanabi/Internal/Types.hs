import Control.Monad (join)
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text as Text
import Hanabi
import qualified Hanabi.Client.Messaging as Client
import Test.QuickCheck
import Test.QuickCheck.Property

main = quickCheck prop_gameEncodes

prop_convertsBetweenRepresentations game =
  again . ioProperty $ do
    let game2 = Client.toHanabi (Client.fromHanabi game)
    if (game2 /= game)
      then (print game2) >> return failed
      else return succeeded

prop_gameEncodes game = Just game == (decodeGame . encode) game
  where
    decodeGame :: ByteString -> Maybe Game
    decodeGame = decode

instance Arbitrary Number where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
  arbitrary = Card (-1) <$> arbitrary <*> arbitrary

instance Arbitrary PlayerId where
  arbitrary = (PlayerId . Text.pack) <$> arbitrary

instance Arbitrary Fact where
  arbitrary =
    (elements [Not, Prelude.id]) <*>
    (join $ elements [IsColor <$> arbitrary, IsNumber <$> arbitrary])

instance Arbitrary Game where
  arbitrary =
    Game <$> maxSized 10 <*> maxSized 5 <*> maxSized 25 <*> maxSized 25 <*>
    maxSized 25 <*>
    maxSized 8 <*>
    maxSized 3 <*>
    maxSized 10

maxSized n = scale (min n) arbitrary
