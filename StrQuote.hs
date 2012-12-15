{- Taken from http://www.haskell.org/haskellwiki/Poor_man's_here_document -}
module StrQuote(str) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
str = QuasiQuoter { quoteExp = stringE, quotePat = litP . stringL, quoteType = \s -> return ListT, quoteDec = \s -> return [] }
