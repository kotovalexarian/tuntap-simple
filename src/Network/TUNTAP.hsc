{-# LANGUAGE ForeignFunctionInterface #-}
module Network.TUNTAP where

import Foreign
import Foreign.C.String

import Data.Bits

import System.Posix.IOCtl

#include <sys/ioctl.h>
#include <sys/socket.h>
#include <linux/if.h>
#include <linux/if_tun.h>

data Flag = TUN | TAP | NO_PI | MULTI_QUEUE
  deriving (Eq)

data Ifreq = Ifreq { flags :: [Flag]
                   , name :: String}

sheet = [ (#{const IFF_TUN}, TUN)
        , (#{const IFF_TAP}, TAP)
        , (#{const IFF_NO_PI}, NO_PI)
        , (#{const IFF_MULTI_QUEUE}, MULTI_QUEUE)
        ]

instance Storable Ifreq where
  alignment _ = #{alignment struct ifreq}
  sizeOf _ = #{size struct ifreq}
  peek ptr = do 
    flags_ <- #{peek struct ifreq, ifr_flags} ptr :: IO (#{type typeof(((struct ifreq *)0)->ifr_flags)})
    let flags = foldr (\a b -> if ((fst a) .&. flags_) /= 0 then (snd a) : b else b) [] sheet 
    name <- peekCString $ #{ptr struct ifreq, ifr_name} ptr
    return (Ifreq flags name)
  poke ptr (Ifreq flags name) = do
    #{poke struct ifreq, ifr_flags} ptr
      ((foldr (\a b -> if elem (snd a) flags then b .|. (fst a) else b) 0 sheet) :: #{type typeof(((struct ifreq *)0)->ifr_flags)})
    withCStringLen (take maxLen name) $ uncurry (copyArray $ #{ptr struct ifreq, ifr_name} ptr)
    where maxLen = #{const IFNAMSIZ}

--foreign import ccall "tunsetiff" tunsetiff :: CInt

data TUNSETIFF = TUNSETIFF

instance IOControl TUNSETIFF Ifreq where
  ioctlReq _ = #{const TUNSETIFF}
