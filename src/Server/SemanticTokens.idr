module Server.SemanticTokens

import Core.Metadata
import Data.List
import Language.LSP.Message
import Libraries.Data.PosMap
import Data.String
import Core.Context

||| encode using relative tokens according the to LSP spec
encode : FilePos -> List ASemanticDecoration -> List Int
encode _ [] = []
encode (relLine, relStartChar) (((_, (sl, sc), (el, ec)), decor, _) :: xs) =
  encoding ++ encode (sl, sc) xs
 where
  ||| Convert Decoration to legend index
  encodeDecorType : Decoration -> Int
  encodeDecorType Typ      = 0
  encodeDecorType Function = 1
  encodeDecorType Data     = 2
  encodeDecorType Bound    = 3
  encodeDecorType Keyword  = 4

  ||| Line, StartChar, Length, TokenType, TokenModifiers
  encoding : List Int
  encoding = [ sl - relLine
             , if sl == relLine then sc - relStartChar else sc
             , ec - sc
             , encodeDecorType decor
             , 0]

||| Remove zero width tokens and split multiline tokens
processToken : (Int -> Int) -> ASemanticDecoration -> List ASemanticDecoration
processToken getLineLength orig@((fileName, (startLine, startChar), (endLine, endChar)), decoration, name) =
  if startLine == endLine
  then
    if startChar == endChar
    then []
    else [orig]
  else
    let
      lineLength = getLineLength startLine
      rest = processToken getLineLength ((fileName, (startLine+1, 0), (endLine, endChar)), decoration, name)
      in if lineLength - startChar == 0 then rest else ((fileName, (startLine, startChar), (startLine, lineLength)), decoration, name) :: rest

||| Write from last to current, poping of the stack when the end of a token is reached
||| current = Nothing means there a no remaining tokens except those on the stack
processStack : (last : FilePos) -> (current : Maybe FilePos) -> (stack : List ASemanticDecoration) -> (List ASemanticDecoration, List ASemanticDecoration)
processStack last _ [] = ([], [])
processStack last mcurrent orig@(((fileName, _, end), decoration, name)::rest) = case (mcurrent, maybe False (end >) mcurrent) of
  (Just current, True) => ([((fileName, last, current), decoration, name)], orig)
  _ => let (output, stack) = processStack end mcurrent rest in (((fileName, last, end), decoration, name) :: output, stack)

||| Remove all overlaping tokens
removeOverlap : (last : FilePos) -> (stack : List ASemanticDecoration) -> (next : List ASemanticDecoration) -> List ASemanticDecoration
removeOverlap last stack [] = fst $ processStack last Nothing stack
removeOverlap last stack (current@((fileName, start, end), decoration, name)::rest) =
  let (output, newStack) = processStack last (Just start) stack
  in output ++ removeOverlap start (current :: newStack) rest

||| Split the remaining base on if they have the same start as the previous token
splitStart : FilePos -> List ASemanticDecoration -> (List ASemanticDecoration, List ASemanticDecoration)
splitStart _ [] = ([], [])
splitStart pstart full@(x@((_, xstart, _), _, _)::rest) = 
  if pstart == xstart
  then
    let (same, newRest) = splitStart pstart rest
    in (x :: same, newRest)
  else ([], full)

||| If two highlights start at the same line ensure the parent is first
||| This step may be able to be removed if changes are made to semantic highlighting
ensureParentFirst : List ASemanticDecoration -> List ASemanticDecoration
ensureParentFirst [] = []
ensureParentFirst full@(x@((_, xstart, _), _, _)::rest) =
  let
    (same, newRest) = splitStart xstart rest
    sortFunction : ASemanticDecoration -> ASemanticDecoration -> Ordering
    sortFunction ((_, _, xend), _, _) ((_, _, yend), _, _) = compare yend xend
  in sortBy sortFunction (x :: same) ++ ensureParentFirst newRest

||| Get the semantic tokens from the Metadata 
export
getSemanticTokens : Metadata -> (getLineLength : Int -> Int) -> SemanticTokens
getSemanticTokens md getLineLength =
  let
    semHigh = md.semanticHighlighting

    aliases : List ASemanticDecoration =
      flip foldMap md.semanticAliases $ \ (from, to) =>
        let decors = uncurry exactRange (snd to) semHigh
        in map (\ ((fnm, loc), rest) => ((fnm, snd from), rest)) decors

    defaults : List ASemanticDecoration =
      flip foldMap md.semanticDefaults $ \ decor@((_, range), _) =>
        case uncurry exactRange range semHigh of
          [] => [decor]
          _ => []
    outOfOrderHighlightingList = toList $ semHigh `union` ((fromList aliases) `union` (fromList defaults))
    overlappingHighlightingList = ensureParentFirst $ outOfOrderHighlightingList
    multilineSemanticHighlightingList = removeOverlap (0, 0) [] $ overlappingHighlightingList
    singlelineSemanticHighlightingList = foldMap (processToken getLineLength) $ multilineSemanticHighlightingList
    encodedTokens = encode (0, 0) singlelineSemanticHighlightingList
    in MkSemanticTokens Nothing encodedTokens
