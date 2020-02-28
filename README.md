# aatbe-lang
![Rust](https://github.com/chronium/aatbe-lang/workflows/Rust/badge.svg)

Åtbe language compiler. Written in Rust.

Anything's A Tuple if you're Brave Enough.

# Usage
```
USAGE:
    aatboot [FLAGS] [OPTIONS] <INPUT>

FLAGS:
    -j, --jit        JIT the code
    -c, --bitcode    Emit LLVM Bitcode
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
        --emit-llvm <LLVM_OUT>          File to output LLVM IR
        --emit-parsetree <PARSE_OUT>    File to output Parse Tree
    -l <lib>...                         Link with library without prefix or extension

ARGS:
    <INPUT>    The file to compile
```

#### You can use `--` to pipe commands from `cargo` to the executable.
```sh
> cargo run -- -j main.aat
# Builds and runs the compiler, piping `-j main.aat` to the compiler's stdin
```

## Example usage:

```sh
> aatboot -j main.aat -lgl -lz -lssl
# Runs main.aat in the JIT while linking with `libgl.so`, `libz.so` and `libssl.so`
```


# Raytracer bench

To run the raytracer bench, you just need to pipe the output to a `.ppm` file.

```sh
> cargo run -- -j bench/raytracer.aat > test.ppm
```

# Compiling to an executable

To compile to an executable, you need to use the `--emit-llvm <filename>.ll` flag, after which, you use clang to compile it.

## Example usage:
```sh
> cargo run -- main.aat --emit-llvm main.ll
# Compilation successful
> clang-6.0 main.ll -o main.out
# main.out is the linked executable
```

#### WARN: Do not use the `-c/--bitcode` flags as they are yet to be implemented.

Related to this, the `kern` branch has a `Makefile` that shows how to automate this task. It's really easy to do.

# Disclaimer
Under no circumstances do I advise anyone to use this in a production environment, or anywhere near any project that has any significant value. It will not catch your computer on fire, but, it might catch your heart on fire <3
