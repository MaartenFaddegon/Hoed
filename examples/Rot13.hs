-- A defective ROT13 implemenation.
import Data.Maybe
import Data.Char
import Debug.Hoed.Pure

main = printO (prop_rot13length "Abc")

-----------------------------------------------------------------------------

rot13     = observe "rot13" rot13'
normalize = observe "normalize" normalize'
rot13char = observe "rot13char" rot13char'

rot13' = mapMaybe (\c -> rot13char (normalize c))
normalize' c = lookup c (zip ['a'..'z'] ['A' .. 'Z'])
rot13char' Nothing = Nothing
rot13char' (Just c) = lookup c table
  where table = zip ['A'..'Z'] (['N'..'Z'] ++ ['A'..'M'])

prop_rot13length s = length t == length (rot13 t)
  where t = filter isAlpha s
