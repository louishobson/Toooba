BUILD_DIR="./../../builds/RV64ACDFIMSUxCHERI_Toooba_bluesim/build_dir"
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR ./../../src_Core/RISCY_OOO/coherence/src/prefetcher/Prefetcher_intf.bsv && \
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR ./../../src_Core/RISCY_OOO/coherence/src/prefetcher/InstructionPrefetchers.bsv && \
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR ./../../src_Core/RISCY_OOO/coherence/src/prefetcher/StridePrefetchers.bsv && \
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR ./../../src_Core/RISCY_OOO/coherence/src/prefetcher/MarkovPrefetchers.bsv && \
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR ./../../src_Core/RISCY_OOO/coherence/src/prefetcher/CheriPrefetchers.bsv && \
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR ./../../src_Core/RISCY_OOO/coherence/src/prefetcher/SignaturePathPrefetcher.bsv && \
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR ./../../src_Core/RISCY_OOO/coherence/src/prefetcher/Prefetcher_top.bsv && \
bsc -bdir $BUILD_DIR -show-range-conflict -simdir $BUILD_DIR -sim -g mkCapBitmapPrefetcherTest3 ./../../src_Testbench/Unit/CheriPrefetcher_test.bsv && \
bsc -bdir $BUILD_DIR -simdir $BUILD_DIR -sim -e mkCapBitmapPrefetcherTest3 -o prefetch_capbitmap_test3 && \ 
./prefetch_capbitmap_test3  && \
rm ./prefetch_capbitmap_test*