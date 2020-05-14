-- ./docker.dhall

let Prelude =
      https://prelude.dhall-lang.org/v11.1.0/package.dhall sha256:99462c205117931c0919f155a6046aec140c70fb8876d208c7c77027ab19c2fa

let Build = 
    { directory : Text
    , tag : Text
    , imageVersion : Text
    , modelVersion : Text
    , binaryVersion : Optional Text
    }

-- other values: "develop," "master"
let defaultImageVersion : Text = "testing" 

let builds : List Build =
    [ 
        { directory = "models/basgra"
        , tag = "pecan/model-basgra-basgra_n_v1.0:" ++ defaultImageVersion
        , imageVersion = defaultImageVersion
        , modelVersion = "BASGRA_N_v1.0"
        , binaryVersion = None Text
        },
        { directory = "models/biocro"
        , tag = "pecan/model-biocro-0.95:" ++ defaultImageVersion
        , imageVersion = defaultImageVersion
        , modelVersion = "BASGRA_N_v1.0"
        , binaryVersion = None Text
        }
    ]

in { builds = builds }
