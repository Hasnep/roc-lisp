default: format check test build run

fix_paths file:
    sd 'import {{ file }}' 'import Unicode.{{ file }}' $(fd --extension .roc . src/lib/Unicode/)
    sd '{{ file }}\.' 'Unicode.{{ file }}.' $(fd --extension .roc . src/lib/Unicode/)

vendor:
    pasta
    just fix_paths CodePoint
    just fix_paths Grapheme
    just fix_paths GraphemeTest
    just fix_paths Helpers
    just fix_paths InternalCP
    just fix_paths InternalEAW
    just fix_paths InternalEmoji
    just fix_paths InternalGBP
    just fix_paths Scalar

format:
    -roc format src/

check:
    roc check src/lib/main.roc
    roc check src/app/main.roc

test:
    roc test src/lib/main.roc
    roc test src/app/main.roc

build:
    mkdir -p build
    roc build --output=build/lisp-interpreter src/app/main.roc

run: build
    build/lisp-interpreter '(+ 1 (* 2 3))'
