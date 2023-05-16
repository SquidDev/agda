{-# LANGUAGE CPP             #-}

#if __GLASGOW_HASKELL__ >= 900
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
#endif

module Agda.VersionCommit where

import Development.GitRev

import Agda.Version

versionWithCommitInfo :: String
versionWithCommitInfo = version ++ maybe "" ("-" ++) commitInfo

-- | Information about current git commit, generated at compile time
commitInfo :: Maybe String
commitInfo = Nothing
