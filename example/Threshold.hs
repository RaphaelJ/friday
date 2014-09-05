import Vision.Image
import System.Environment (getArgs)

main :: IO ()
main = do
    [input, output] <- getArgs
    io <- either (\x -> error $ "Load failed: " ++ show x) return =<< load Nothing input
    let grey = convert io :: Grey
    _ <- save output (otsu grey)
    return ()
