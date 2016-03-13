{-# LANGUAGE CPP #-}

module Episodes.StaticFiles where

import Prelude
import Yesod.EmbeddedStatic

#ifdef DEVELOPMENT
#define DEV_BOOL True
#else
#define DEV_BOOL False
#endif

mkEmbeddedStatic DEV_BOOL "episodesStatic"
    [ embedDirAt "css" "static/css"
    , embedDirAt "js" "static/js"
    , embedDirAt "img" "static/img" ]
