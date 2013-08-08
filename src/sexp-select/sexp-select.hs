module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative  ((<$>),(*>),(<*),(<*>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Sexp ( parseExn, printHum, printMach )
import Data.Sexp (Sexp(..))
import System.Environment (getArgs)
import Text.Printf
import Data.Maybe (catMaybes)
import Data.Char (isUpper)

main = do
  selector <- parseSelector . head <$> getArgs
  sexps <- parseExn <$> BS.getContents
  mapM_ (BS.putStrLn . printMach) $ catMaybes $ map (select selector) sexps
  
data Selector = 
  Nth Int -- '[0]'
  | Named BS.ByteString 
  | Any -- '*'
  | Descendant Selector Selector -- 'foo bar'
  | Child Selector Selector -- 'foo > bar'
    deriving Show
                                                   
parseSelector str = 
  case parse parser "" str of
    Right s -> s
    Left err -> error $ show err
  where
    parser = spaces *> selector <* eof
    selector = (try child <|> try descendant <|> simple) <* spaces 
    simple = (nth <|> wildcard <|> named) <* spaces
    nth = Nth . read <$> (char '[' *> many1 digit <* char ']')
    wildcard = char '*' >> return Any
    descendant = Descendant <$> simple <*> selector
    child = Child <$> simple <*> (char '>' >> spaces *> selector)
    named = Named . BS.pack <$> many1 alphaNum
      
-- detect a special case of sexp that represents a record
isRecord (List [Atom recordName, List fields]) | isUpper (BS.head recordName) = True
isRecord _ = False

-- Only call this on records
recFields (List [Atom recordName, List fields]) = fields
recName   (List [Atom recordName, List fields]) = recordName

select (Nth idx) (Atom _)   = fail (printf "Cannot select element %d from atom" idx)
select (Nth idx) (List lst) = return $ lst!!idx

select Any sexp = return sexp

select (Named name) (Atom _) = fail "Cannot select named field from atom"
-- Special case for record field
select (Named name) (List [Atom key, value]) | key == name = return value
-- Special case for selecting whole record by record type name
select sel@(Named name) sexp@(List lst)
  | isRecord sexp =
    if name == recName sexp 
    then return $ List $ recFields sexp
    else selectAll sel (recFields sexp)
  | otherwise = selectAll sel lst

select (Descendant sel1 sel2) sexp = do
  candidate <- select sel1 sexp
  select sel2 candidate

select (Child sel1 sel2) sexp = do
  candidate <- select sel1 sexp
  case select sel2 candidate of
    Just result -> return result
    Nothing -> 
      case candidate of
        (Atom _) -> fail "nowhere to descend"
        maybeRec@(List lst) ->
          selectAll (Child Any sel2) $ if isRecord maybeRec
                                       then recFields maybeRec
                                       else lst

selectAll selector sexps =
  let vals = catMaybes $ map (select selector) sexps in
  case vals of
    [] -> fail "nothing found"
    [c] -> return c
    cs -> return (List cs)
