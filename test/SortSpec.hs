module SortSpec where

import Model
import Test.Hspec

spec :: Spec
spec = describe "Sorting" $ do
  describe "parseSortParam" $ do
    it "parses valid sort parameters" $ do
      parseSortParam (Just "title:asc") `shouldBe` Just (M.SortBy "title" M.Asc)
      parseSortParam (Just "title:desc") `shouldBe` Just (M.SortBy "title" M.Desc)

    it "defaults to ascending when no order specified" $ do
      parseSortParam (Just "title") `shouldBe` Just (M.SortBy "title" M.Asc)

    it "returns Nothing for invalid input" $ do
      parseSortParam (Just "invalid:badorder") `shouldBe` Nothing
      parseSortParam Nothing `shouldBe` Nothing
