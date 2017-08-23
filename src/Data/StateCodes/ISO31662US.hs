{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}

-- | This file is generated from the Wikipedia page
-- <https://en.wikipedia.org/wiki/ISO_3166-2:US>

module Data.StateCodes.ISO31662US
  ( StateCode(..)
  , stateList
  , fromMName
  , fromMText
  , fromName
  , fromText
  , toName
  , toText
  ) where

import           Control.Applicative   (pure)
import           Data.Aeson
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Typeable
import           Text.Shakespeare.I18N

data StateCode = AL
               | AK
               | AZ
               | AR
               | CA
               | CO
               | CT
               | DE
               | FL
               | GA
               | HI
               | ID
               | IL
               | IN
               | IA
               | KS
               | KY
               | LA
               | ME
               | MD
               | MA
               | MI
               | MN
               | MS
               | MO
               | MT
               | NE
               | NV
               | NH
               | NJ
               | NM
               | NY
               | NC
               | ND
               | OH
               | OK
               | OR
               | PA
               | RI
               | SC
               | SD
               | TN
               | TX
               | UT
               | VT
               | VA
               | WA
               | WV
               | WI
               | WY
               | DC
               | AS
               | GU
               | MP
               | PR
               | UM
               | VI
               deriving (Bounded, Eq, Enum, Show, Read, Ord, Typeable)


-- | Maybe get the StateCode from the text code.

fromMText :: Text -> Maybe StateCode
fromMText "AL" = Just AL
fromMText "AK" = Just AK
fromMText "AZ" = Just AZ
fromMText "AR" = Just AR
fromMText "CA" = Just CA
fromMText "CO" = Just CO
fromMText "CT" = Just CT
fromMText "DE" = Just DE
fromMText "FL" = Just FL
fromMText "GA" = Just GA
fromMText "HI" = Just HI
fromMText "ID" = Just ID
fromMText "IL" = Just IL
fromMText "IN" = Just IN
fromMText "IA" = Just IA
fromMText "KS" = Just KS
fromMText "KY" = Just KY
fromMText "LA" = Just LA
fromMText "ME" = Just ME
fromMText "MD" = Just MD
fromMText "MA" = Just MA
fromMText "MI" = Just MI
fromMText "MN" = Just MN
fromMText "MS" = Just MS
fromMText "MO" = Just MO
fromMText "MT" = Just MT
fromMText "NE" = Just NE
fromMText "NV" = Just NV
fromMText "NH" = Just NH
fromMText "NJ" = Just NJ
fromMText "NM" = Just NM
fromMText "NY" = Just NY
fromMText "NC" = Just NC
fromMText "ND" = Just ND
fromMText "OH" = Just OH
fromMText "OK" = Just OK
fromMText "OR" = Just OR
fromMText "PA" = Just PA
fromMText "RI" = Just RI
fromMText "SC" = Just SC
fromMText "SD" = Just SD
fromMText "TN" = Just TN
fromMText "TX" = Just TX
fromMText "UT" = Just UT
fromMText "VT" = Just VT
fromMText "VA" = Just VA
fromMText "WA" = Just WA
fromMText "WV" = Just WV
fromMText "WI" = Just WI
fromMText "WY" = Just WY
fromMText "DC" = Just DC
fromMText "AS" = Just AS
fromMText "GU" = Just GU
fromMText "MP" = Just MP
fromMText "PR" = Just PR
fromMText "UM" = Just UM
fromMText "VI" = Just VI
fromMText _    = Nothing


-- | Get the StateCode from the text code. Errors if the code is unknown

fromText :: Text -> StateCode
fromText c = case fromMText c of
               Just sc -> sc
               _       -> error $ "fromText: Unknown state code:" ++ T.unpack c


-- | Get the code as text

toText :: StateCode -> Text
toText AL = "AL"
toText AK = "AK"
toText AZ = "AZ"
toText AR = "AR"
toText CA = "CA"
toText CO = "CO"
toText CT = "CT"
toText DE = "DE"
toText FL = "FL"
toText GA = "GA"
toText HI = "HI"
toText ID = "ID"
toText IL = "IL"
toText IN = "IN"
toText IA = "IA"
toText KS = "KS"
toText KY = "KY"
toText LA = "LA"
toText ME = "ME"
toText MD = "MD"
toText MA = "MA"
toText MI = "MI"
toText MN = "MN"
toText MS = "MS"
toText MO = "MO"
toText MT = "MT"
toText NE = "NE"
toText NV = "NV"
toText NH = "NH"
toText NJ = "NJ"
toText NM = "NM"
toText NY = "NY"
toText NC = "NC"
toText ND = "ND"
toText OH = "OH"
toText OK = "OK"
toText OR = "OR"
toText PA = "PA"
toText RI = "RI"
toText SC = "SC"
toText SD = "SD"
toText TN = "TN"
toText TX = "TX"
toText UT = "UT"
toText VT = "VT"
toText VA = "VA"
toText WA = "WA"
toText WV = "WV"
toText WI = "WI"
toText WY = "WY"
toText DC = "DC"
toText AS = "AS"
toText GU = "GU"
toText MP = "MP"
toText PR = "PR"
toText UM = "UM"
toText VI = "VI"


-- | Maybe get the code from the user readable name

fromMName :: Text -> Maybe StateCode
fromMName "Alabama"                              = Just AL
fromMName "Alaska"                               = Just AK
fromMName "Arizona"                              = Just AZ
fromMName "Arkansas"                             = Just AR
fromMName "California"                           = Just CA
fromMName "Colorado"                             = Just CO
fromMName "Connecticut"                          = Just CT
fromMName "Delaware"                             = Just DE
fromMName "Florida"                              = Just FL
fromMName "Georgia"                              = Just GA
fromMName "Hawaii"                               = Just HI
fromMName "Idaho"                                = Just ID
fromMName "Illinois"                             = Just IL
fromMName "Indiana"                              = Just IN
fromMName "Iowa"                                 = Just IA
fromMName "Kansas"                               = Just KS
fromMName "Kentucky"                             = Just KY
fromMName "Louisiana"                            = Just LA
fromMName "Maine"                                = Just ME
fromMName "Maryland"                             = Just MD
fromMName "Massachusetts"                        = Just MA
fromMName "Michigan"                             = Just MI
fromMName "Minnesota"                            = Just MN
fromMName "Mississippi"                          = Just MS
fromMName "Missouri"                             = Just MO
fromMName "Montana"                              = Just MT
fromMName "Nebraska"                             = Just NE
fromMName "Nevada"                               = Just NV
fromMName "New Hampshire"                        = Just NH
fromMName "New Jersey"                           = Just NJ
fromMName "New Mexico"                           = Just NM
fromMName "New York"                             = Just NY
fromMName "North Carolina"                       = Just NC
fromMName "North Dakota"                         = Just ND
fromMName "Ohio"                                 = Just OH
fromMName "Oklahoma"                             = Just OK
fromMName "Oregon"                               = Just OR
fromMName "Pennsylvania"                         = Just PA
fromMName "Rhode Island"                         = Just RI
fromMName "South Carolina"                       = Just SC
fromMName "South Dakota"                         = Just SD
fromMName "Tennessee"                            = Just TN
fromMName "Texas"                                = Just TX
fromMName "Utah"                                 = Just UT
fromMName "Vermont"                              = Just VT
fromMName "Virginia"                             = Just VA
fromMName "Washington"                           = Just WA
fromMName "West Virginia"                        = Just WV
fromMName "Wisconsin"                            = Just WI
fromMName "Wyoming"                              = Just WY
fromMName "District of Columbia"                 = Just DC
fromMName "American Samoa"                       = Just AS
fromMName "Guam"                                 = Just GU
fromMName "Northern Mariana Islands"             = Just MP
fromMName "Puerto Rico"                          = Just PR
fromMName "United States Minor Outlying Islands" = Just UM
fromMName "Virgin Islands, U.S."                 = Just VI
fromMName _                                      = Nothing


-- | Get the StateCode from the user readable name. Errors if the name is unknown

fromName:: Text -> StateCode
fromName s = case fromMName s of
               Just sc -> sc
               _       -> error $ "fromName: Unknown state code:" ++ T.unpack s


-- | Get the user readable name

toName :: StateCode -> Text
toName AL = "Alabama"
toName AK = "Alaska"
toName AZ = "Arizona"
toName AR = "Arkansas"
toName CA = "California"
toName CO = "Colorado"
toName CT = "Connecticut"
toName DE = "Delaware"
toName FL = "Florida"
toName GA = "Georgia"
toName HI = "Hawaii"
toName ID = "Idaho"
toName IL = "Illinois"
toName IN = "Indiana"
toName IA = "Iowa"
toName KS = "Kansas"
toName KY = "Kentucky"
toName LA = "Louisiana"
toName ME = "Maine"
toName MD = "Maryland"
toName MA = "Massachusetts"
toName MI = "Michigan"
toName MN = "Minnesota"
toName MS = "Mississippi"
toName MO = "Missouri"
toName MT = "Montana"
toName NE = "Nebraska"
toName NV = "Nevada"
toName NH = "New Hampshire"
toName NJ = "New Jersey"
toName NM = "New Mexico"
toName NY = "New York"
toName NC = "North Carolina"
toName ND = "North Dakota"
toName OH = "Ohio"
toName OK = "Oklahoma"
toName OR = "Oregon"
toName PA = "Pennsylvania"
toName RI = "Rhode Island"
toName SC = "South Carolina"
toName SD = "South Dakota"
toName TN = "Tennessee"
toName TX = "Texas"
toName UT = "Utah"
toName VT = "Vermont"
toName VA = "Virginia"
toName WA = "Washington"
toName WV = "West Virginia"
toName WI = "Wisconsin"
toName WY = "Wyoming"
toName DC = "District of Columbia"
toName AS = "American Samoa"
toName GU = "Guam"
toName MP = "Northern Mariana Islands"
toName PR = "Puerto Rico"
toName UM = "United States Minor Outlying Islands"
toName VI = "Virgin Islands, U.S."


-- | List of names sorted by alphabetical order, with state code
-- this is ready to be used in a yesod selectField, for example

stateList :: [(T.Text, StateCode)]
stateList = [ ("Alabama", AL)
            , ("Alaska", AK)
            , ("Arizona", AZ)
            , ("Arkansas", AR)
            , ("California", CA)
            , ("Colorado", CO)
            , ("Connecticut", CT)
            , ("Delaware", DE)
            , ("Florida", FL)
            , ("Georgia", GA)
            , ("Hawaii", HI)
            , ("Idaho", ID)
            , ("Illinois", IL)
            , ("Indiana", IN)
            , ("Iowa", IA)
            , ("Kansas", KS)
            , ("Kentucky", KY)
            , ("Louisiana", LA)
            , ("Maine", ME)
            , ("Maryland", MD)
            , ("Massachusetts", MA)
            , ("Michigan", MI)
            , ("Minnesota", MN)
            , ("Mississippi", MS)
            , ("Missouri", MO)
            , ("Montana", MT)
            , ("Nebraska", NE)
            , ("Nevada", NV)
            , ("New Hampshire", NH)
            , ("New Jersey", NJ)
            , ("New Mexico", NM)
            , ("New York", NY)
            , ("North Carolina", NC)
            , ("North Dakota", ND)
            , ("Ohio", OH)
            , ("Oklahoma", OK)
            , ("Oregon", OR)
            , ("Pennsylvania", PA)
            , ("Rhode Island", RI)
            , ("South Carolina", SC)
            , ("South Dakota", SD)
            , ("Tennessee", TN)
            , ("Texas", TX)
            , ("Utah", UT)
            , ("Vermont", VT)
            , ("Virginia", VA)
            , ("Washington", WA)
            , ("West Virginia", WV)
            , ("Wisconsin", WI)
            , ("Wyoming", WY)
            , ("District of Columbia", DC)
            , ("American Samoa", AS)
            , ("Guam", GU)
            , ("Northern Mariana Islands", MP)
            , ("Puerto Rico", PR)
            , ("United States Minor Outlying Islands", UM)
            , ("Virgin Islands, U.S.", VI)
            ]


-- | To JSON: as a simple string

instance ToJSON StateCode where
  toJSON = toJSON . toText


-- | From JSON: as a simple string

instance FromJSON StateCode where
  parseJSON (String s)
    | Just a <- fromMText s = pure a
  parseJSON _          = fail "StateCode"


-- | Show user readable name, in English (ignoring locale for now)

instance RenderMessage master StateCode where
  renderMessage _ _ = toName
