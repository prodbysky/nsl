# nsl - Compiled programming language

So far you can define assign, and return values.
Also you can do basic arithmetic (only integers for now)

## Examples
 - Full language (for now since it is pretty small)
```nsl
    let a = 5;
    let b = a + 5;

    if a < b {
        return 1;
    }

    return 0;
```



## Why?
I want to make a compiled, statically typed python.
Also implementing a language and later using it, being in control,
having a finished project seems fun.


## Misc. info
Uses gcc for linking (for early dev, later will use ld)

## TODO:
 - Functions (definitions, calls)
 - Good integration with C code
 - Module system to avoid the C/C++ pitfall of header files
 - ADT's
 - Not ugly (yet functional) syntax
 - Decent errors (Looking at you C++ templates)
 - Good standard library (networking, graphics (probably raylib), collections)
 - If there will be one, a build system that's written in nsl (similar to build.zig and Tsodings nob.h)
 - Type safety
 - Type annotations
 - Trait-like interfaces
 - Dead code analysis for functions, structs, variables, constants

## Non-concerns for this language:
 - For now (maybe it will change later) this is not Rust, C, C++, it will not be as quick as those languages
 - OOP - no inheritance, however structures will be able to have associated methods
 - Being crossplatform (making a language is hard enough)
