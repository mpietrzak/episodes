{-# LANGUAGE CPP #-}

module Settings.StaticFiles where


import Prelude
import Yesod.EmbeddedStatic
import Yesod.PureScript.EmbeddedGenerator



#ifdef DEVELOPMENT
#define DEV_BOOL True
#else
#define DEV_BOOL False
#endif


mkEmbeddedStatic DEV_BOOL "myStatic"
    [ purescript "js/Episodes.js" defaultPsGeneratorOptions { psProductionMinimizer = return
                                                            , psSourceDirectory = "purs" }
    , embedDirAt "css" "static/css"
    , embedDirAt "js" "static/js"
    , embedDirAt "img" "static/img"
    ]
