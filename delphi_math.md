# Delphi 2010 Math Notes

**Functions covered:** `sqr`, `sqrt`, `trunc`, `floor`, `ceil`, `Power` (often written as “pwr”), `round`, and **how to round to N decimals**.

> Simple wording: we’ll just say **int** for whole numbers and **real** for decimal numbers.

---

## Setup

Add this at the top of your unit:

```pascal
uses
  SysUtils,   // Writeln, FormatFloat, etc.
  Math;       // floor, ceil, Power, RoundTo, SimpleRoundTo
```

---

## `sqr(x)` — square a number

- **You send:** an **int** or a **real**  
- **You get back:** the **same kind** (int in → int out, real in → real out)  
- **What it does:** `x * x`

**Examples**

```pascal
Writeln( sqr(5) );     // 25      (int → int)
Writeln( sqr(1.5) );   // 2.25    (real → real)
Writeln( sqr(-3) );    // 9
```

**Tip:** Very big ints can overflow. If numbers might be large, use real values (e.g., `5.0`).

---

## `sqrt(x)` — square root

- **You send:** a **real** that is **≥ 0**
- **You get back:** a **real**
- **What it does:** returns √x

**Examples**

```pascal
Writeln( sqrt(9.0) );  // 3.0
Writeln( sqrt(2.0) );  // 1.414213562...
Writeln( sqrt(0.0) );  // 0.0
// sqrt(-1.0) is invalid → handle that yourself before calling
```

**Safe check example**

```pascal
function SafeSqrt(const X: real): real;
begin
  if X < 0 then
    Result := 0 // or any value you prefer
  else
    Result := sqrt(X);
end;
```

---

## `trunc(x)` — drop decimals toward 0

- **You send:** a **real**
- **You get back:** an **int**
- **What it does:** removes the decimal part and moves **toward 0**

**Examples**

```pascal
Writeln( trunc( 3.9 ) );  // 3
Writeln( trunc(-3.9 ) );  // -3   (toward 0)
Writeln( trunc( 2.0 ) );  // 2
```

---

## `floor(x)` — round down (toward −∞)

- **You send:** a **real**
- **You get back:** an **int**
- **What it does:** largest whole number **≤ x**

**Examples**

```pascal
Writeln( floor( 3.9 ) );  // 3
Writeln( floor(-3.1 ) );  // -4   (downward)
Writeln( floor( 2.0 ) );  // 2
```

**Compare with `trunc` on negatives**

- `trunc(-3.9) = -3` (toward 0)  
- `floor(-3.9) = -4` (down)

---

## `ceil(x)` — round up (toward +∞)

- **You send:** a **real**
- **You get back:** an **int**
- **What it does:** smallest whole number **≥ x**

**Examples**

```pascal
Writeln( ceil( 3.1 ) );   // 4
Writeln( ceil(-3.1 ) );   // -3  (up toward +∞)
Writeln( ceil( 2.0 ) );   // 2
```

---

## `Power(base, exponent)` — raise to a power (often called “pwr”)

- **You send:** two **reals**: `base` and `exponent`
- **You get back:** a **real**
- **What it does:** `base ^ exponent`

**Examples**

```pascal
Writeln( Power(2.0, 10.0) );  // 1024.0
Writeln( Power(9.0, 0.5) );   // 3.0   (square root)
Writeln( Power(27.0, 1.0/3) );// ~3.0  (cube root)
Writeln( Power(10.0, -2.0) ); // 0.01
```

**Faster when exponent is a whole number:** use `IntPower`

```pascal
Writeln( IntPower(3, 4) );    // 81  (int exponent)
Writeln( IntPower(3.0, 4) );  // 81.0
```

---

## `round(x)` — nearest int (Delphi’s default is “banker’s rounding”)

- **You send:** a **real**
- **You get back:** an **int**
- **What it does:** rounds to the nearest whole number.  
  If exactly halfway (`.5`), Delphi’s `round` goes to the **even** number.

**Examples**

```pascal
Writeln( round( 3.49 ) );   // 3
Writeln( round( 3.50 ) );   // 4   (nearest; .5 goes to even)
Writeln( round( 2.50 ) );   // 2   (2 is even)
Writeln( round(-2.50 ) );   // -2  (even)
```

**If you want “.5 always up” (away from zero):** use `SimpleRoundTo` (see below).

---

## How to round to N decimals

Use `RoundTo` (Delphi’s banker’s rounding) or `SimpleRoundTo` (always away from zero). Both are in `Math`.

### 1) `RoundTo(Value, Exponent)`

- **You send:** a **real** value, and an **exponent** (an int from −37..+37)
- **You get back:** a **real**
- **How the exponent works:**
  - `-1` → 1 decimal place  
  - `-2` → 2 decimal places  
  - `0`  → whole number  
  - `+1` → nearest 10, `+2` → nearest 100, etc.

**Examples (banker’s rounding)**

```pascal
Writeln( RoundTo(123.4567, -2) );  // 123.46
Writeln( RoundTo(123.445 , -2) );  // 123.44  (.005 ties go to even)
Writeln( RoundTo(15.0    , +1) );  // 20.0    (nearest 10)
```

### 2) `SimpleRoundTo(Value, Exponent)`

- **You send:** a **real** value, and an **exponent**
- **You get back:** a **real**
- **Rule:** .5 **always** goes **away from zero**

**Examples (away-from-zero)**

```pascal
Writeln( SimpleRoundTo(123.445, -2) ); // 123.45
Writeln( SimpleRoundTo(-1.25  , -1) ); // -1.3
```

### 3) Just for display (don’t change the value)

Use `FormatFloat` to **show** N decimals:

```pascal
Writeln( FormatFloat('0.00', 123.456) );      // "123.46"
Writeln( FormatFloat('#,##0.000', 9876.5) );  // "9,876.500"
```

---

## Demo program

```pascal

program DemoMath2010;
{$APPTYPE CONSOLE}

uses
  SysUtils, Math;

procedure Run;
var
  x: real;
begin
  x := -3.9;

  Writeln('sqr(5)         = ', sqr(5));                                       // 25
  Writeln('sqrt(2)        = ', FormatFloat('0.000000', sqrt(2)));
  Writeln('trunc(-3.9)    = ', trunc(x));                                   // -3
  Writeln('floor(-3.9)    = ', floor(x));                                  // -4
  Writeln('ceil(-3.1)     = ', ceil(-3.1));                               // -3
  Writeln('Power(2,10)    = ', Power(2,10));                             // 1024
  Writeln('IntPower(3,4)  = ', IntPower(3,4));                          // 81
  Writeln('round(2.5)     = ', round(2.5));                            // 2 
  Writeln('round(3.5)     = ', round(3.5));                           // 4

  // Rounding to N decimals
  Writeln('RoundTo(123.4567,-2)      = ', RoundTo(123.4567, -2):0:2);      // 123.46
  Writeln('SimpleRoundTo(123.445,-2) = ', SimpleRoundTo(123.445, -2):0:2); // 123.45

  // Formatting for display
  Writeln('FormatFloat 2dp: ', FormatFloat('0.00', 123.456)); // "123.46"
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```

---

## Cheat Sheet

| Function  | Send         | Returns | What it does                                  |
|-----------|--------------|---------|------------------------------------------------|
| `sqr(x)`  | int or real  | same    | x × x                                         |
| `sqrt(x)` | real ≥ 0     | real    | square root of x                              |
| `trunc(x)`| real         | int     | drop decimals toward 0                        |
| `floor(x)`| real         | int     | biggest int ≤ x (down)                        |
| `ceil(x)` | real         | int     | smallest int ≥ x (up)                         |
| `Power(a,b)`| real, real | real    | a to the power of b                           |
| `IntPower(a,n)`| a: int/real, n: int | int/real | a to an integer power (faster for whole n) |
| `round(x)`| real         | int     | nearest int, `.5` goes to **even**            |
| `RoundTo(v,e)` | real, int | real  | banker’s rounding to decimals / tens etc.     |
| `SimpleRoundTo(v,e)` | real, int | real | `.5` always **away from 0**                 |

**Remember**

- Use `RoundTo(value, -N)` or `SimpleRoundTo(value, -N)` to get **N decimals**.
- Use `FormatFloat` if you only want to **show** N decimals without changing the value.
- For negative numbers, learn the difference between `trunc`, `floor`, and `ceil`.
