import Control.Monad.State
import Debug.Trace

type Gensym a = State Int a
type GensymT m a = StateT Int m a

gensym :: (Monad m) => GensymT m String
gensym = do
  x <- get
  put (x+1)
  return $ "_sym" ++ show x

type GL a = GensymT [] a
-- GL a = StateT Int [] a
--     ~= Int -> [(Int, a)]

make :: Int -> GL String
make n = do
  xs <- replicateM n gensym
  lift $ xs

distro :: [Int] -> String -> GL String
distro [] s = return s
distro (n:ns) s = do
  x <- make n
  xs <- distro ns s
  return $ x ++ xs

test as = runStateT (distro as "hoge") 0

main = do
  print $ test [1, 2, 3, 4]
