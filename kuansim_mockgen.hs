#!/usr/bin/runghc 
{-# LANGUAGE FlexibleInstances, OverloadedStrings, UndecidableInstances, DeriveGeneric, DeriveDataTypeable#-} 
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import System.Environment
import Data.Data
import Data.Aeson
import Data.Typeable
import GHC.Generics

chichars_surname = map BSUTF8.fromString $ words "李 王 張 劉 陳 楊 黃 趙 周 吳 徐 孫 朱 馬 胡 郭 林 何 高 梁 鄭 羅 宋 謝 唐 韓 曹 許 鄧 蕭 馮 曾 程 蔡 彭 潘 袁 於 董 餘 蘇 葉 呂 魏 蔣 田 杜 丁 沈 姜 範 江 傅 鐘 盧 汪 戴 崔 任 陸 廖 姚 方 金 邱 夏 譚 韋 賈 鄒 石 熊 孟 秦 閻 薛 侯 雷 白 龍 段 郝 孔 邵 史 毛 常 萬 顧 賴 武 康 賀 嚴 尹 錢 施 牛 洪 龔"
chichars_givenname = map BSUTF8.fromString $ words "世 中 仁 伶 佩 佳 俊 信 倫 偉 傑 儀 元 冠 凱 君 哲 嘉 國 士 如 娟 婷 子 孟 宇 安 宏 宗 宜 家 建 弘 強 彥 彬 德 心 志 忠 怡 惠 慧 慶 憲 成 政 敏 文 昌 明 智 曉 柏 榮 欣 正 民 永 淑 玉 玲 珊 珍 珮 琪 瑋 瑜 瑞 瑩 盈 真 祥 秀 秋 穎 立 維 美 翔 翰 聖 育 良 芬 芳 英 菁 華 萍 蓉 裕 豪 貞 賢 郁 鈴 銘 雅 雯 霖 青 靜 韻 鴻 麗 龍" 

data User = User 
    { surename :: BSUTF8.ByteString
    , givename :: GiveName
    , email :: Email
    , prefs :: UserPrefers} 
            deriving (Generic, Data,Typeable,Show)

data GiveName = GiveName [BSUTF8.ByteString]
              deriving (Generic, Data,Typeable,Show)

data Email = Email 
           { id :: String 
           , host :: String }
           deriving (Generic, Data, Typeable, Show)
-- Generator
instance Arbitrary User where
    arbitrary = liftM4 User surename arbitrary arbitrary arbitrary
            where surename = elements chichars_surname

data UserPrefers = UserPrefers
    { prefer_poston :: BSUTF8.ByteString }
    deriving (Data,Typeable,Show, Generic)

instance Arbitrary GiveName where
    arbitrary = do
      k <- choose (1, 2)
      a <- vectorOf k $ elements chichars_givenname 
      return (GiveName a)

instance Arbitrary Email where
    arbitrary = do
                host <- elements ["gmail.com", "yahoo.com", "hotmail.com", "msn.com"]
                k <- choose (4, 10)
                id <- vectorOf k $ elements ['a'..'z']
                return (Email id host)
                              
instance Arbitrary UserPrefers where
    arbitrary = liftM UserPrefers poston
        where poston = elements ["facebook", "twitter", "plurk", "googleplus"]

userGen :: Int -> [User]
userGen seed = unGen arbitrary (mkStdGen seed) 9999999

-- JSON 
instance ToJSON User
instance ToJSON UserPrefers
instance ToJSON Email where
    toJSON (Email id host) = object ["email" .= (id ++ "@" ++ host) ]
instance ToJSON GiveName where
    toJSON (GiveName lst) = object ["givename" .= BS.concat lst]

main = do
  args <- getArgs
  case map read args of
    [] -> putStrLn "usage: [user number]"   
    [un] -> go un 2
  where 
    go un seed = do
         BL.writeFile "data/kuansim/users.json" $ encode users
             where users = take un $ userGen seed
