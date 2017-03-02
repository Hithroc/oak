module Oak.Web.Utils (toHashid, fromHashid) where
import Web.Hashids (encode, decode, hashidsSimple)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Spoon (spoon)
import Data.Maybe (listToMaybe)
import Control.Monad

hashidsContext = hashidsSimple ""

toHashid :: Int -> Text
toHashid = decodeUtf8 . encode hashidsContext

fromHashid :: Text -> Maybe Int
fromHashid str =  listToMaybe 
              <=< spoon 
               .  decode hashidsContext
               .  encodeUtf8
               $  str
