import Control.Applicative
import Data.Char
import System.Environment

rotChar :: Char -> Int -> Char
rotChar c n | isAlpha c = let base = ord $ if isLower c then 'a' else 'A'
                          in chr $ (ord c - base + n) `mod` 26 + base
            | otherwise = c

rotStr :: String -> Int -> String
rotStr s n = map (flip rotChar n) s

rots :: String -> [String]
rots s = flip rotStr <$> [0..25] <*> pure s

main = do as <- getArgs
          ws <- readFile "/usr/share/dict/words"
          mapM_ putStrLn
                [s | s <- rots $ unwords as,
                     all (`elem` lines ws) $
                         words $ map toLower $ filter ( or
                                                      . (<*>) [isAlpha, isSpace]
                                                      . pure
                                                      ) s]
