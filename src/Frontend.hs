module Frontend(runServer) where

import Web.Scotty
import Text.RawString.QQ
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.Foldable (toList)

import Lexer

import Parser
import GraphReduction

runServer :: IO ()
runServer =
  scotty 3000 $ do
    middleware $ staticPolicy (addBase "assets")

    get "/" $
      html
        [r|
          <html lang="en">
          <head>
            <meta charset="UTF-8">
            <meta http-equiv="X-UA-Compatible" content="IE=edge">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <link
              rel="stylesheet" 
              href="https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css"
            />
            <title>Capybara</title>
            <style>
              img {
                width: 60px;
                border-radius: 50px;
                float: left;
                margin: 5px;
              }
            </style>
          </head>
          <body>
            <img src="capybara.jpg" alt="capybara pic">
            <h1>Capy-Lambda</h1>
            <form action="/result" method="GET">
                <input type="text" name="expr" />
                <button type="submit">Evaluate</button>
            </form>
          </body>
          </html>
        |]
    
    get "/result" $ do
      expr <- param "expr"
      case runParser abstraction (tokenize expr) of
        Left err -> html $ TL.pack ("<p>invalid input: " <> err <> "</p>")
        Right (ast, _) ->
          let listElems = TL.concat ["<li>" <> TL.pack (show graph) <> "</li>" | graph <- map toList (run ast)]
          in html $
            [r|
              <!DOCTYPE html>
              <html lang="en">
              <head>
                <meta charset="UTF-8">
                <meta http-equiv="X-UA-Compatible" content="IE=edge">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <title>result</title>
              </head>
              <body>
                  <ul> |]
            <> listElems
            <> [r|
                  </ul>
              </body>
              </html>
            |]
        