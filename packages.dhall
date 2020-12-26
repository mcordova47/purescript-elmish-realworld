let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.8-20201223/src/packages.dhall sha256:a1a8b096175f841c4fef64c9b605fb0d691229241fd2233f6cf46e213de8a185

let additions =
      { elmish =
          https://raw.githubusercontent.com/collegevine/purescript-elmish/master/elmish.dhall sha256:b09a2cec99cd53d59399ad9eb2cf0fe923da7d6a80c58d21d3ef881ecd582a6b
            "v0.2.2"
      }

let overrides = {=}

in  upstream // additions // overrides
