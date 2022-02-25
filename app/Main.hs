module Main where

import Frontend


-- main :: IO ()
-- main = case runParser abstraction (tokenize "(\\x.(x x))(\\y.y)") of
--     Left err -> putStrLn err
--     Right (ast, _) ->
--         let graphs = map toList (run ast)
--         in mapM_ print graphs

main :: IO ()
main = runServer

-- some lambda expression
-- (\\x.(x x))(\\y.y)
-- ((\\x.\\y.x)((\\w.w)(\\z.z)))(\\u.u)
-- \\f.(\\x.(f (x x))) (\\x.(f (x x)))