import Data.ByteString.Lazy
import Hanabi
import Data.Aeson
import Test.QuickCheck
import Data.Text as Text
import Control.Monad (join)

main = quickCheck prop_gameEncodes

prop_gameEncodes game = Just game == (decodeGame . encode) game
  where
    decodeGame :: ByteString -> Maybe Game
    decodeGame = decode

instance Arbitrary Number where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary PlayerId where
  arbitrary = (PlayerId . Text.pack) <$> arbitrary

instance Arbitrary Fact where
  arbitrary =
    (elements [Not, id]) <*>
    (join $ elements [IsColor <$> arbitrary, IsNumber <$> arbitrary])

instance Arbitrary Game where
  arbitrary =
    Game <$> maxSized 10 <*> maxSized 5 <*> maxSized 25 <*> maxSized 25 <*>
    maxSized 25 <*>
    maxSized 8 <*>
    maxSized 3 <*>
    maxSized 10

maxSized n = scale (min n) arbitrary
