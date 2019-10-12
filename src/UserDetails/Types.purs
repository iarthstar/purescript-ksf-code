module UserDetails.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Axios (Config(..), Header(..), Method(..), axios)
import Axios (class Axios, genericAxios)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

newtype LoginReq = LoginReq 
  { username :: String
  , password :: String
  }
derive instance genericLoginReq :: Generic LoginReq _
instance encodeLoginReq :: Encode LoginReq where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

newtype LoginRes = LoginRes 
  { uuid :: String
  , token :: String
  , ssoCode :: String
  }
derive instance newtypeLoginRes :: Newtype LoginRes _
derive instance genericLoginRes :: Generic LoginRes _
instance decodeLoginRes :: Decode LoginRes where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

-- | Axios instance for Usage API
instance axiosUsageReq :: Axios LoginReq LoginRes 
  where axios = genericAxios



newtype DetailsReq = DetailsReq {}
derive instance genericDetailsReq :: Generic DetailsReq _
instance encodeDetailsReq :: Encode DetailsReq where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

newtype DetailsRes = DetailsRes 
  { firstName :: String
  , lastName :: String
  , address :: 
      { streetAddress :: String
      , streetName :: String
      , zipCode :: String
      , countryCode :: String
      }
  }
derive instance newtypeDetailsRes :: Newtype DetailsRes _
derive instance genericDetailsRes :: Generic DetailsRes _
instance decodeDetailsRes :: Decode DetailsRes where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

-- | Axios instance for Usage API
instance axiosGetDetails :: Axios DetailsReq DetailsRes 
  where axios = genericAxios 

newtype PatchDetailsReq = PatchDetailsReq
  { firstName :: String
  , lastName :: String
  , address :: 
      { streetAddress :: String
      , streetName :: String
      , zipCode :: String
      , countryCode :: String
      }
  }
derive instance genericPatchDetailsReq :: Generic PatchDetailsReq _
instance encodePatchDetailsReq :: Encode PatchDetailsReq where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

newtype PatchDetailsRes = PatchDetailsRes { firstName :: String }
derive instance newtypePatchDetailsRes :: Newtype PatchDetailsRes _
derive instance genericPatchDetailsRes :: Generic PatchDetailsRes _
instance decodePatchDetailsRes :: Decode PatchDetailsRes where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

-- | Axios instance for Usage API
instance axiosPatchDetails :: Axios PatchDetailsReq PatchDetailsRes 
  where axios = genericAxios 