# Compilers, Compile-Time Errors, and Runtime Errors

---

## What is a compiler?

A **compiler** is a program that translates human-readable source code (e.g., C, C++, Rust, Swift, Java) into a lower-level form the machine can execute (machine code or bytecode). The output is typically:

- a native **executable** (e.g., `.exe`, ELF), or
- **object files** that are later **linked** into an executable or library, or
- **bytecode** (e.g., Java `.class`) that runs on a virtual machine (JVM).

Compilers often do **static checks** (syntax, types), **optimize** code, and **emit diagnostics** to help you fix issues before running the program.

Basicly a compiler converts your code into 1's and 0's so the computor can understand it

---

## What is a compile-time error?

An error detected **before the program runs**—during compilation. The compiler refuses to produce a runnable artifact.

Basicly it is when your program wont run because there are errors.

### Common causes

- **Syntax errors:** Missing braces, keywords, punctuation.
- **Type errors:** Assigning a `String` to an `int`, calling a function with wrong argument types.
- **Name resolution errors:** Unknown identifiers, missing imports.
- **Link-time errors:** Unresolved external symbols, multiple definitions, missing libraries.

### Why they matter

- They prevent faulty programs from shipping.
- They’re usually **deterministic** and reproducible without running the program.

#### Example (Java – type error)

```java

// Compile-time error: incompatible types
int x = "hello"; // String cannot be converted to int

```

---

## What is a runtime error?

An error that occurs **while the program is running**, after it has successfully compiled. Often depends on **inputs**, **environment**, or **state** that the compiler cannot fully predict.

### Most Common causes

- **Illegal operations:** Divide by zero, invalid casts.
- **Resource failures:** Out of memory, file not found, network timeouts.
- **Null/None dereferences:** Accessing missing objects.
- **Indexing errors:** Array/collection out-of-bounds.
- **Undefined behavior (low-level languages):** Use-after-free, data races.

### Example (Python: Runtime Exception)

```java
// Compiles fine, fails at runtime
x = input("Enter a number: ") // The user enters 0
y = 12
z = y / x
```

---

## Key differences at a glance

| Aspect                | Compile-Time Error                                   | Runtime Error                                          |
|-----------------------|------------------------------------------------------|--------------------------------------------------------|
| When it occurs        | During build (compile time)                          | While the program is executing                         |
| Detects               | Syntax, type, missing symbols                        | Invalid ops, bad inputs, resource/IO failures          |
| Blocks program build? | **Yes**                                              | No (program builds and starts)                         |
| Reproducibility       | High (independent of inputs)                         | Often input/data/environment-dependent                 |
| Typical artifact      | No binary / build fails                              | Binary runs, then crashes/throws exceptions            |

---

## Reducing each kind of error

### To Reduce Compile-Time Errors

- Use a **strict type system** and fix all warnings.
- Keep **build scripts** and **dependencies** consistent (lockfiles, pinned versions).
- Employ **linters** and **formatters**.

### To Reduce Runtime Errors

- Validate all **inputs** (never trust user/network).
- Check **return values** and handle **exceptions**.
- Add **timeouts/retries** for IO; guard memory and indices.
- Use **tests** (unit/integration/property/fuzzing) and **observability** (logging, metrics, tracing).

---

### TL;DR

- **Compiler:** Optimises your code and then translates it into 1's and 0's
- **Compile-time errors:** Found during build; block the program from running.  
- **Runtime errors:** Happen while running; depend on inputs/environment.

---
