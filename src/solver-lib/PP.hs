module PP (ppProg) where

import Types
import Text.PrettyPrint

ppProg :: ExpC -> String
ppProg = render . ppProgDoc

ppProgDoc :: ExpC -> Doc
ppProgDoc e = parens $ text "lambda" <+> parens (text "x") <+> ppExpCDoc e

ppExpCDoc (ExpC _ e) = ppExpDoc e

ppExpDoc :: Exp -> Doc
ppExpDoc e =
  case e of
    Zero -> int 0
    One -> int 1
    MainArg -> text "x"
    Fold1Arg -> text "foldElem"
    Fold2Arg -> text "foldAcc"
    If cond t f ->
      parens $ text "if0" <+> ppExpCDoc cond <+> ppExpCDoc t <+> ppExpCDoc f
    Fold a s b ->
      let
        lambda = parens $
          text "lambda" <+>
          (parens $ text "foldElem" <+> text "foldAcc") <+>
          ppExpCDoc b
      in parens $ text "fold" <+> ppExpCDoc a <+> ppExpCDoc s <+> lambda
    Not e -> parens $ text "not" <+> ppExpCDoc e
    Shl1 e -> parens $ text "shl1" <+> ppExpCDoc e
    Shr1 e -> parens $ text "shr1" <+> ppExpCDoc e
    Shr4 e -> parens $ text "shr4" <+> ppExpCDoc e
    Shr16 e -> parens $ text "shr16" <+> ppExpCDoc e
    And e1 e2 -> parens $ text "and" <+> ppExpCDoc e1 <+> ppExpCDoc e2
    Or e1 e2 -> parens $ text "or" <+> ppExpCDoc e1 <+> ppExpCDoc e2
    Xor e1 e2 -> parens $ text "xor" <+> ppExpCDoc e1 <+> ppExpCDoc e2
    Plus e1 e2 -> parens $ text "plus" <+> ppExpCDoc e1 <+> ppExpCDoc e2
