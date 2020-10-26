# aatbe-lang
![Rust](https://github.com/chronium/aatbe-lang/workflows/Rust/badge.svg)

Åtbe language compiler. Written in Rust.

Anything's A Tuple if you're Brave Enough.

Goals:
* Fully featured standard library, similar to how Rust's stdlib is.
* Clean and easy syntax, similar to Scala and Ruby
* Easy to pick up, even for a person who has no experience with programming.

# Discord

Feel free to join the Discord, you can find more updates there, a lot more often, and we have a great community of developers: https://discord.gg/25zTv9M

# Requirements
You need the latest Rust Nightly.
The following are the required ubuntu packages: `llvm-dev`, `clang`, some form of build essentials.

# Usage
```
USAGE:
    aatboot [FLAGS] [OPTIONS] <INPUT>

FLAGS:
    -c, --bitcode    Emit LLVM Bitcode
    -j, --jit        JIT the code
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -l <LIB>...                         Link with library without prefix or extension
        --emit-llvm <LLVM_OUT>          File to output LLVM IR
    -o <OUT_FILE>                       File to output the compiled code
        --emit-parsetree <PARSE_OUT>    File to output Parse Tree
        --stdlib <STDLIB>               Set the Aatbe stdlib path

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

```sh
> aatbot main.aat -o main
or
> cargo run -- main.aat -o main
```

The shown command will compile `main.aat` and link it using `clang` into a working executable.

# Disclaimer
Under no circumstances do I advise anyone to use this in a production environment, or anywhere near any project that has any significant value. It will not catch your computer on fire, but, it might catch your heart on fire <3
