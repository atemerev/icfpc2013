module PP (ppProg) where

import Types
import Text.PrettyPrint

ppProg :: Exp -> String
ppProg = render . ppProgDoc

ppProgDoc :: Exp -> Doc
ppProgDoc e = parens $ text "lambda" <+> parens (text "x") <+> ppExpDoc e

ppExpDoc :: Exp -> Doc
ppExpDoc e =
  case e of
    Zero -> int 0
    One -> int 1
    MainArg -> text "x"
    Fold1Arg -> text "foldElem"
    Fold2Arg -> text "foldAcc"
    If cond t f ->
      parens $ text "if0" <+> ppExpDoc cond <+> ppExpDoc t <+> ppExpDoc f
    Fold a s b ->
      let
        lambda = parens $
          text "lambda" <+>
          (parens $ text "foldElem" <+> text "foldAcc") <+>
          ppExpDoc b
      in parens $ text "fold" <+> ppExpDoc a <+> ppExpDoc s <+> lambda
    Not e -> parens $ text "not" <+> ppExpDoc e
    Shl1 e -> parens $ text "shl1" <+> ppExpDoc e
    Shr1 e -> parens $ text "shr1" <+> ppExpDoc e
    Shr4 e -> parens $ text "shr4" <+> ppExpDoc e
    Shr16 e -> parens $ text "shr16" <+> ppExpDoc e
    And e1 e2 -> parens $ text "and" <+> ppExpDoc e1 <+> ppExpDoc e2
    Or e1 e2 -> parens $ text "or" <+> ppExpDoc e1 <+> ppExpDoc e2
    Xor e1 e2 -> parens $ text "xor" <+> ppExpDoc e1 <+> ppExpDoc e2
    Plus e1 e2 -> parens $ text "plus" <+> ppExpDoc e1 <+> ppExpDoc e2
