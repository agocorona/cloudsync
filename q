name: CloudSync
version: 0.0.1
cabal-version: >=1.2
build-type: Simple
license: AllRightsReserved
license-file: ""
homepage: syb
synopsis: Sync data in the cloud using event sourcing
description:  
data-dir: ""
 
library
    build-depends: RefSerialize -any, TCache -any, Workflow -any,
                   base -any, binary -any, bytestring -any, containers -any,
                   distributed-process -any, distributed-process-simplelocalnet -any, ghc-prim -any
                   distributed-static -any, hashtables -any, network -any,
                   old-time -any, primitive -any, stm -any, syb -any,
                   template-haskell -any
    exposed-modules: CloudSync EventSourcing
    exposed: True
    buildable: True
    cpp-options: -DMAIN_FUNCTION=testMain
    hs-source-dirs: src
 
test-suite test-CloudSync
    build-depends: RefSerialize -any, TCache -any, Workflow -any,
                   base -any, binary -any, bytestring -any, containers -any,
                   distributed-process -any, distributed-process-simplelocalnet -any,
                   distributed-static -any, hashtables -any, network -any,
                   old-time -any, primitive -any, stm -any, syb -any,
                   template-haskell -any
    type: exitcode-stdio-1.0
    main-is: Main.hs
    buildable: True
    cpp-options: -DMAIN_FUNCTION=testMain
    hs-source-dirs: src
