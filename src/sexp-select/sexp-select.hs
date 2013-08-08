module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative  ((<$>),(*>),(<*),(<*>))
import Data.ByteString.Lazy.Char8 as BS hiding ( head, map, concatMap, concat )
import Language.Sexp ( parseExn, printHum, printMach )
import Data.Sexp (Sexp(..))
import System.Environment (getArgs)
import Text.Printf
import Data.Maybe (catMaybes)

main = do
  selector <- parseSelector . head <$> getArgs
  sexp <- parseExn <$> BS.getContents
  mapM_ (BS.putStrLn . printMach) $ concatMap (select selector) sexp
  
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
isRecord (List [Atom _recordname, List fields]) = True
isRecord _ = False

-- Only call this on records
recFields (List [Atom recordName, List fields]) = fields
recName   (List [Atom recordName, List fields]) = recordName

select (Nth idx) (Atom _) = fail (printf "Cannot select element %d from atom" idx)
select (Nth idx) sexp@(List lst) 
  | isRecord sexp = return $ (recFields sexp)!!idx
  | otherwise     = return $ lst!!idx

select Any sexp = return sexp

select (Named name) (Atom _) = fail "Cannot select named field from atom"
-- Special case for selecting whole record by record type name
select (Named name) sexp@(List lst)
  | isRecord sexp =
    if name == recName sexp then recFields sexp
    else 
      case catMaybes (map isNamed (recFields sexp)) of
        []  -> fail "boo"
        [f] -> return f
        fs  -> return $ List fs
  where
    isNamed (List [Atom key, value]) | key == name = Just value
    isNamed _ = Nothing

select (Descendant sel1 sel2) sexp =
  case [ candidate | candidate <- select sel1 sexp ] of
    []  -> fail "no matching descendants"
    [c] -> select sel2 c
    cs  -> select sel2 (List cs)

select (Child sel1 sel2) sexps =
  concat [ case select sel2 candidate of
              [] -> 
                case candidate of
                  (List _) -> select (Child Any sel2) candidate
                  (Atom _) -> []
              results -> results
         | candidate <- select sel1 sexps ]
