GCD in BF code
Get your two numbers (leave space at the beginning just in case):
>>>> , > ,

Example Algorithm:
gcd a b =
    a == b ? b :? gcd (b `mod` a) a

So first check if a == 0 and return b if so
Thankfully this matches brainfuck loop structure

Initial: a !b 0 0 0 0 0
Copy a over (skip five): a b 0 0 0 a 0 0 0
Copy and destroy a over 5 cells
<
[->>>>>+>+<<<<<<]>>>>>>[-<<<<<<+>>>>>>]<<<<<<

Copy b over (skip one): !a b 0 b 0 a 0 0 0
>
[->>+>+<<<]>>>[-<<<+>>>]<<<

Put an indicator value for debugging:
a !b 0 b 0 a 0 0 0 "0 13"
>>>>>>>>>+++++++++++++<<<<<<<<<<

Otherwise repeat with a = b `mod` a and b = a
Start on a
[ while a != 0
    a isn't a; now it's b so shift over to where we stored that 2nd b
    a b 0 !b 0 a 0 0 0
    >>>

    Run modulus algorithm
    [>+>->+<[>]>[<+>-]<<[<]>-]

    Result is: a b 0 !0 b a_b%a b%a 0 0
    <<<

    We want the new memory to look like:
    b%a a 0 a 0 b%a 0 0 0
    and we have
    a! b 0 0 b a_b%a b%a 0 0
    
    So first clear that b and then copy a over then copy a over skipping two then move back to the start and clear a and clear b
    >[-]<
    [->+>+<<]>>[-<<+>>]<<
    >>>>[-]<<<<
    !a a 0 0 0 a_b%a b%a 0 0
    [->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<<
    [-]
    >>>>
    0 a 0 a !0 a_b%a b%a 0 0

    Now clear a_b%a and copy b%a left
    >[-]>
    [-<+<+>>]<<[->>+<<]>>
    0 a 0 a 0 b%a !b%a 0 0

    Copy b%a to start then clear original b%a and move back to start
    [-<<<<<<+<+>>>>>>>]<<<<<<<[->>>>>>>+<<<<<<<]>>>>>>>
    [-]
    <<<<<<
    !b%a a 0 a 0 b%a 0 0 0
]

print b: >.
