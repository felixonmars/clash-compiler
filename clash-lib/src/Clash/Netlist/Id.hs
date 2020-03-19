{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transform/format a Netlist Identifier so that it is acceptable as a HDL identifier
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Id
  ( IdType (..)
  , Identifier
  , mkIdentifier

  , mkBasicId'
  , stripDollarPrefixes
  )
where

import Clash.Annotations.Primitive (HDL (..))
import Control.Applicative ((<|>))
import Control.Applicative.Extra (orEmpty)
import Control.Arrow (second)
import Control.DeepSeq (force)
import Data.Char (isAsciiLower,isAsciiUpper,isDigit)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text as Text
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Text.Read (readMaybe)
import GHC.Stack (HasCallStack)

data IdType = Basic | Extended

-- | Time units: are added to 'reservedWords' as simulators trip over signals
-- named after them.
vhdlTimeUnits :: [Text]
vhdlTimeUnits = ["fs", "ps", "ns", "us", "ms", "sec", "min", "hr"]

-- List of reserved VHDL-2008 keywords
-- + used internal names: toslv, fromslv, tagtoenum, datatotag
-- + used IEEE library names: integer, boolean, std_logic, std_logic_vector,
--   signed, unsigned, to_integer, to_signed, to_unsigned, string
vhdlKeywords :: [Text]
vhdlKeywords = ["abs","access","after","alias","all","and","architecture"
  ,"array","assert","assume","assume_guarantee","attribute","begin","block"
  ,"body","buffer","bus","case","component","configuration","constant","context"
  ,"cover","default","disconnect","downto","else","elsif","end","entity","exit"
  ,"fairness","file","for","force","function","generate","generic","group"
  ,"guarded","if","impure","in","inertial","inout","is","label","library"
  ,"linkage","literal","loop","map","mod","nand","new","next","nor","not","null"
  ,"of","on","open","or","others","out","package","parameter","port","postponed"
  ,"procedure","process","property","protected","pure","range","record"
  ,"register","reject","release","rem","report","restrict","restrict_guarantee"
  ,"return","rol","ror","select","sequence","severity","signal","shared","sla"
  ,"sll","sra","srl","strong","subtype","then","to","transport","type"
  ,"unaffected","units","until","use","variable","vmode","vprop","vunit","wait"
  ,"when","while","with","xnor","xor","toslv","fromslv","tagtoenum","datatotag"
  ,"integer", "boolean", "std_logic", "std_logic_vector", "signed", "unsigned"
  ,"to_integer", "to_signed", "to_unsigned", "string","log"] <> vhdlTimeUnits

-- List of reserved Verilog-2005 keywords
verilogKeywords :: [Text]
verilogKeywords =
  ["always","and","assign","automatic","begin","buf","bufif0"
  ,"bufif1","case","casex","casez","cell","cmos","config","deassign","default"
  ,"defparam","design","disable","edge","else","end","endcase","endconfig"
  ,"endfunction","endgenerate","endmodule","endprimitive","endspecify"
  ,"endtable","endtask","event","for","force","forever","fork","function"
  ,"generate","genvar","highz0","highz1","if","ifnone","incdir","include"
  ,"initial","inout","input","instance","integer","join","large","liblist"
  ,"library","localparam","macromodule","medium","module","nand","negedge"
  ,"nmos","nor","noshowcancelled","not","notif0","notif1","or","output"
  ,"parameter","pmos","posedge","primitive","pull0","pull1","pulldown","pullup"
  ,"pulsestyle_onevent","pulsestyle_ondetect","rcmos","real","realtime","reg"
  ,"release","repeat","rnmos","rpmos","rtran","rtranif0","rtranif1","scalared"
  ,"showcancelled","signed","small","specify","specparam","strong0","strong1"
  ,"supply0","supply1","table","task","time","tran","tranif0","tranif1","tri"
  ,"tri0","tri1","triand","trior","trireg","unsigned","use","uwire","vectored"
  ,"wait","wand","weak0","weak1","while","wire","wor","xnor","xor"]

-- List of reserved SystemVerilog-2012 keywords
systemVerilogKeywords :: [Text]
systemVerilogKeywords =
  ["accept_on","alias","always","always_comb","always_ff"
  ,"always_latch","and","assert","assign","assume","automatic","before","begin"
  ,"bind","bins","binsof","bit","break","buf","bufif0","bufif1","byte","case"
  ,"casex","casez","cell","chandle","checker","class","clocking","cmos","config"
  ,"const","constraint","context","continue","cover","covergroup","coverpoint"
  ,"cross","deassign","default","defparam","design","disable","dist","do","edge"
  ,"else","end","endcase","endchecker","endclass","endclocking","endconfig"
  ,"endfunction","endgenerate","endgroup","endinterface","endmodule","endpackage"
  ,"endprimitive","endprogram","endproperty","endspecify","endsequence"
  ,"endtable","endtask","enum","event","eventually","expect","export","extends"
  ,"extern","final","first_match","for","force","foreach","forever","fork"
  ,"forkjoin","function","generate","genvar","global","highz0","highz1","if"
  ,"iff","ifnone","ignore_bins","illegal_bins","implements","implies","import"
  ,"incdir","include","initial","inout","input","inside","instance","int"
  ,"integer","interconnect","interface","intersect","join","join_any"
  ,"join_none","large","let","liblist","library","local","localparam","logic"
  ,"longint","macromodule","matches","medium","modport","module","nand"
  ,"negedge","nettype","new","nexttime","nmos","nor","noshowcancelled","not"
  ,"notif0","notif1","null","or","output","package","packed","parameter","pmos"
  ,"posedge","primitive","priority","program","property","protected","pull0"
  ,"pull1","pulldown","pullup","pulsestyle_ondetect","pulsestyle_onevent"
  ,"pure","rand","randc","randcase","randsequence","rcmos","real","realtime"
  ,"ref","reg","reject_on","release","repeat","restrict","return","rnmos"
  ,"rpmos","rtran","rtranif0","rtranif1","s_always","s_eventually","s_nexttime"
  ,"s_until","s_until_with","scalared","sequence","shortint","shortreal"
  ,"showcancelled","signed","small","soft","solve","specify","specparam"
  ,"static","string","strong","strong0","strong1","struct","super","supply0"
  ,"supply1","sync_accept_on","sync_reject_on","table","tagged","task","this"
  ,"throughout","time","timeprecision","timeunit","tran","tranif0","tranif1"
  ,"tri","tri0","tri1","triand","trior","trireg","type","typedef","union"
  ,"unique","unique0","unsigned","until","until_with","untyped","use","uwire"
  ,"var","vectored","virtual","void","wait","wait_order","wand","weak","weak0"
  ,"weak1","while","wildcard","wire","with","within","wor","xnor","xor"]

reservedKeywords :: HashSet Text
reservedKeywords =
  HashSet.fromList (vhdlKeywords <> verilogKeywords <> systemVerilogKeywords)

-- | Signal reference. Consists of a base name and a number of extensions. An
-- identifier with a base name of "foo" and a list of extensions [1, 2] will be
-- rendered as "foo_1_2". Note that the list of extensions is stored in reverse.
-- Constructor and fields are purposely not exported as to keep the invariants
-- on the fields.
--
-- Note: The Eq instance of "Identifier" is case insensitive! E.g., two
-- identifiers with base names 'fooBar' and 'FoObAR' are considered the same.
-- However, identifiers are stored case preserving. This means Clash won't
-- generate two identifiers with differing case, but it will try to keep
-- capitalization.
--
-- The goal of this data structure is to greatly simplify how Clash deals with
-- identifiers internally. Any Identifier should be trivially printable to any
-- HDL. Additionally, it should be easily extendable and
data Identifier = Identifier
  { i_baseName :: !Text
  -- ^ Base name of identifier. 'mkIdentifier' makes sure this field:
  --
  --    * does not end in '_num' where 'num' is a digit.
  --    * is solely made up of printable ASCII characters
  --    * has no leading or trailing whitespace
  --    * is not a reserved keyword in VHDL, Verilog, SystemVerilog
  --
  , i_extensions :: [Word]
  -- ^ Extensions applied to base identifier. E.g., an identifier with a base
  -- name of 'foo' and an extension of [5, 6] would render as 'foo_5_6'.
  , i_isBasic :: !Bool
  -- ^ If true, this identifier adheres to the pattern '[a-zA-Z](_[a-zA-Z0-9]+)*'.
  -- I.e., starts with letter, followed by letters, digits, and underscores. An
  -- underscore can't be followed by another underscore, nor can it be the last
  -- character.
  --
  -- A basic identifier doesn't have to be escaped in Verilog, VHDL, or
  -- SystemVerilog.
  , i_hashCache :: !Int
  -- ^ Return value of `hash (x :: Identifier)`.
  } deriving (Show)

instance Hashable Identifier where
  hashWithSalt salt = hashWithSalt salt . hash
  hash = i_hashCache

instance Eq Identifier where
  i1 == i2 = (i_baseName i1, i_extensions i1) == (i_baseName i2, i_extensions i2)
  i1 /= i2 = (i_baseName i1, i_extensions i1) /= (i_baseName i2, i_extensions i2)

hashCache# :: Text -> [Word] -> Int
hashCache# baseName extensions =
  -- 'hash' has an identity around zero, e.g. `hash (0, 2) == 2`. Because a lot
  -- of zeros can be expected, extensions are fuzzed in order to keep efficient
  -- `HashMap`s.
  let fuzz fuzzFactor ext = fuzzFactor * fuzzFactor * ext in
  hash (baseName, List.foldl' fuzz 2 extensions)

-- | Does this text follow '[a-zA-Z](_[a-zA-Z0-9]+)*'? If so, it's a valid
-- VHDL (and {System}Verilog) basic identifier. See:
--
--  * http://vhdl.renerta.com/mobile/source/vhd00037.htm
--  * http://www.verilog.renerta.com/source/vrg00018.htm
--
isBasic# :: Text -> Bool
isBasic# = Maybe.isJust . parseId
 where
  parseSingle pred s = do
    (l, ls) <- Text.uncons s
    orEmpty (pred l) ls

  parseLetter = parseSingle (\c -> Char.isAscii c && Char.isLetter c)
  parseDigit = parseSingle Char.isDigit
  parseLetterOrDigit s = parseLetter s <|> parseDigit s
  parseUnderscore = parseSingle (=='_')

  failNonEmpty "" = Just ""
  failNonEmpty _ = Nothing

  repeatParse parser s0 =
    case parser s0 of
      Just s1 -> repeatParse parser s1
      Nothing -> Just s0

  parseGroup s0 = do
    s1 <- parseUnderscore s0 <|> Just s0
    s2 <- parseLetterOrDigit s1
    repeatParse parseLetterOrDigit s2

  parseId s0 = do
    s1 <- parseLetter s0
    s2 <- repeatParse parseGroup s1
    failNonEmpty s2

-- | Convert given string to ASCII. Throws out Unicode mark characters, control
-- characters. Retains all printable ASCII. If a character is neither a Unicode
-- mark, control character, or printable ASCII character, it is replaced with
-- a question mark.
toPrintableAscii# :: Text -> Text
toPrintableAscii# = Text.pack . Maybe.mapMaybe go . Text.unpack
 where
  go c | Char.isMark c = Nothing
       | Char.isControl c = Nothing
       | Char.isPrint c && Char.isAscii c = Just c
       | otherwise = Just '?'

-- | Append '_r' if given identifier is a reserved keyword in some HDL
appendReserved# :: Text -> Text
appendReserved# t = if t `HashSet.member` reservedKeywords then t <> "_r" else t

-- | Split identifiers such as "foo_1_2" into ("foo", [1, 2]).
parseIdentifier# :: HasCallStack => Text -> (Text, [Word])
parseIdentifier# t =
  let (tsRev, extsRev) = go (List.reverse (Text.splitOn "_" t)) in
  (Text.intercalate "_" (List.reverse tsRev), List.reverse extsRev)
 where
  go :: [Text] -> ([Text], [Word])
  go [] = error ("Internal error: invalid identifier in parseIdentifier#" ++ show t)
  go (i:is) = case readMaybe @Word (Text.unpack i) of
    Just w -> second (w:) (go is)
    Nothing -> (is, [])

mkIdentifier :: HasCallStack => Text -> Identifier
mkIdentifier = go . appendReserved# . Text.strip . toPrintableAscii#
 where
  go s | Text.null s = error "Empty (or non-printable) identifier given"
       | otherwise =
          let
            (baseName, extensions) = parseIdentifier# s
            hashCache = hashCache# baseName extensions
            isBasic = isBasic# baseName
          in
            Identifier baseName extensions isBasic hashCache

mkBasicId'
  :: HDL
  -> Bool
  -> Text
  -> Text
mkBasicId' hdl tupEncode = stripMultiscore hdl . stripLeading hdl . zEncode hdl tupEncode
  where
    stripLeading VHDL = Text.dropWhile (`elem` ('_':['0'..'9']))
    stripLeading _    = Text.dropWhile (`elem` ('$':['0'..'9']))
    stripMultiscore VHDL
      = Text.concat
      . Prelude.map (\cs -> case Text.head cs of {'_' -> "_"; _ -> cs})
      . Text.group
    stripMultiscore _ = id

stripDollarPrefixes :: Text -> Text
stripDollarPrefixes = stripWorkerPrefix . stripSpecPrefix . stripConPrefix
                    . stripWorkerPrefix . stripDictFunPrefix
  where
    stripDictFunPrefix t = maybe t (takeWhileEnd (/='_')) (Text.stripPrefix "$f" t)
    stripWorkerPrefix t = Maybe.fromMaybe t (Text.stripPrefix "$w" t)
    stripConPrefix t = Maybe.fromMaybe t (Text.stripPrefix "$c" t)
    stripSpecPrefix t = Maybe.fromMaybe t (Text.stripPrefix "$s" t)

type UserString    = Text -- As the user typed it
type EncodedString = Text -- Encoded form

zEncode :: HDL -> Bool -> UserString -> EncodedString
zEncode hdl False cs = go (uncons cs)
  where
    go Nothing         = empty
    go (Just (c,cs'))  = append (encodeDigitCh hdl c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = append (encodeCh hdl c) (go' $ uncons cs')

zEncode hdl True cs = case maybeTuple cs of
                    Just (n,cs') -> append n (go' (uncons cs'))
                    Nothing      -> go (uncons cs)
  where
    go Nothing         = empty
    go (Just (c,cs'))  = append (encodeDigitCh hdl c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = case maybeTuple (cons c cs') of
                           Just (n,cs2) -> append n (go' $ uncons cs2)
                           Nothing      -> append (encodeCh hdl c) (go' $ uncons cs')

encodeDigitCh :: HDL -> Char -> EncodedString
encodeDigitCh _   c | isDigit c = Text.empty -- encodeAsUnicodeChar c
encodeDigitCh hdl c             = encodeCh hdl c

encodeCh :: HDL -> Char -> EncodedString
encodeCh hdl c | unencodedChar hdl c = singleton c     -- Common case first
               | otherwise           = Text.empty

unencodedChar :: HDL -> Char -> Bool   -- True for chars that don't need encoding
unencodedChar hdl c  =
  or [ isAsciiLower c
     , isAsciiUpper c
     , isDigit c
     , if hdl == VHDL then c == '_' else c `elem` ['_','$']
     ]

maybeTuple :: UserString -> Maybe (EncodedString,UserString)
maybeTuple "(# #)" = Just ("Unit",empty)
maybeTuple "()"    = Just ("Unit",empty)
maybeTuple (uncons -> Just ('(',uncons -> Just ('#',cs))) =
  case countCommas 0 cs of
    (n,uncons -> Just ('#',uncons -> Just (')',cs'))) -> Just (pack ("Tup" ++ show (n+1)),cs')
    _ -> Nothing
maybeTuple (uncons -> Just ('(',cs)) =
  case countCommas 0 cs of
    (n,uncons -> Just (')',cs')) -> Just (pack ("Tup" ++ show (n+1)),cs')
    _ -> Nothing
maybeTuple _  = Nothing

countCommas :: Int -> UserString -> (Int,UserString)
countCommas n (uncons -> Just (',',cs)) = countCommas (n+1) cs
countCommas n cs                        = (n,cs)
