{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables #-}
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Aeson (ToJSON, toJSON, object, encode, (.=))
import Data.DateTime (DateTime)
import System.Random 
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import Data.Typeable (Typeable)
import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5)
import GHC.Generics (Generic)
import System.Environment (getArgs)

data User = User
    { name :: UserName
    , email :: String
    , prefer_poston :: String
    , date_registered :: DateTime}
    deriving (Generic, Typeable, Show)

instance Arbitrary User where
    arbitrary = liftM4 User arbitrary email prefer_poston arbitrary
	where email = do
                host <- elements ["gmail.com", "yahoo.com", "hotmail.com", "msn.com"]
                k <- choose (4, 10)
                id <- vectorOf k $ elements ['a'..'z']
                return (id ++ "@" ++ host)
              prefer_poston = elements ["facebook", "googleplus", "twitter", "plurk"]

instance ToJSON User 

-- User Name
data UserName = ChineseName
    { surname :: String
    , givenname :: ChineseGivenName} deriving (Generic, Typeable)

type ChineseGivenName = [String]

chichars_surname = words "李 王 張 劉 陳 楊 黃 趙 周 吳 徐 孫 朱 馬 胡 郭 林 何 高 梁 鄭 羅 宋 謝 唐 韓 曹 許 鄧 蕭 馮 曾 程 蔡 彭 潘 袁 於 董 餘 蘇 葉 呂 魏 蔣 田 杜 丁 沈 姜 範 江 傅 鐘 盧 汪 戴 崔 任 陸 廖 姚 方 金 邱 夏 譚 韋 賈 鄒 石 熊 孟 秦 閻 薛 侯 雷 白 龍 段 郝 孔 邵 史 毛 常 萬 顧 賴 武 康 賀 嚴 尹 錢 施 牛 洪 龔"
chichars_givenname = words "世 中 仁 伶 佩 佳 俊 信 倫 偉 傑 儀 元 冠 凱 君 哲 嘉 國 士 如 娟 婷 子 孟 宇 安 宏 宗 宜 家 建 弘 強 彥 彬 德 心 志 忠 怡 惠 慧 慶 憲 成 政 敏 文 昌 明 智 曉 柏 榮 欣 正 民 永 淑 玉 玲 珊 珍 珮 琪 瑋 瑜 瑞 瑩 盈 真 祥 秀 秋 穎 立 維 美 翔 翰 聖 育 良 芬 芳 英 菁 華 萍 蓉 裕 豪 貞 賢 郁 鈴 銘 雅 雯 霖 青 靜 韻 鴻 麗 龍"

instance Arbitrary UserName where
    arbitrary = do 
      k <- choose (1, 2)
      s <- elements chichars_surname
      g <- vectorOf k $ elements chichars_givenname
      return (ChineseName s g)

instance Show UserName where
    show (ChineseName s g) = s ++ concat g

instance ToJSON UserName where
    toJSON (ChineseName s g) = object ["surname" .= s
                                      , "givenname" .= concat g]

gendata n = take n $ unGen arbitrary (mkStdGen 2) 9999999

genjson :: String -> Int -> IO ()
genjson t n = BL.writeFile ("data/kuansim/" ++ t ++ ".json") $ encode $ mkdata t
    where mkdata "users" = gendata n :: [User] 
          mkdata _ = error "unsupported type"

data Bookmark = Bookmark
    { author :: User 
    , resolved_item :: ResolvedItem
    , date_resovled :: DateTime
    , in_inbox :: Bool}
    deriving (Generic, Typeable, Show)

instance Arbitrary Bookmark where
    arbitrary = liftM4 Bookmark arbitrary arbitrary arbitrary opt_in_inbox
                where opt_in_inbox = (frequency [(9, arbitrary), (1, arbitrary)])

data ResolvedItem = ResolvedItem
    { resovled_type :: String
    , resolved_type :: Int}
    deriving (Generic, Typeable, Show)

instance Arbitrary ResolvedItem where
    arbitrary = do
      _type <- elements ["news_article"]
      _id <- case _type of 
               "news_article" -> choose (2, 163151)
      return (ResolvedItem _type _id)
               
load_bookmark_pool :: [ResolvedItem]
load_bookmark_pool = gendata 10

usage = "usage: entry_type numner"

main = do
  args <- getArgs
  case map read args of
    [t, un] -> genjson "users" un
    _ -> putStrLn usage
