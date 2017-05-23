/** An 8-bit unsigned integer. */
declare type byte = number;
declare type u8 = number;

/** An 8-bit signed integer. */
declare type sbyte = number;
declare type s8 = number;

/** A 16-bit signed integer. */
declare type short = number;
declare type s16 = number;

/** A 16-bit unsigned integer. */
declare type ushort = number;
declare type u16 = number;

/** A 32-bit signed integer. */
declare type int = number;
declare type s32 = number;

/** A 32-bit unsigned integer. */
declare type uint = number;
declare type u32 = number;

/** A 64-bit signed integer. */
declare type long = number;
declare type s64 = number;

/** A 64-bit unsigned integer. */
declare type ulong = number;
declare type u64 = number;

/** A 32-bit float. */
declare type float = number;
declare type f32 = number;

/** A 64-bit float. */
declare type double = number;
declare type f64 = number;

/** A 32-bit unsigned integer when targeting WASM32 respectively a 64-bit unsigned integer when targeting WASM64. */
declare type intptr = number;

/** A class describing a pointer to a data structure. */
declare class IntPtr<T extends number | object> {
    public offset: intptr;
    public value: T;
    constructor(offset: intptr);
    public increment(diff: intptr): this;
    public decrement(diff: intptr): this;
}

/** Retrieves the byte size of a data structure. */
declare function sizeof<T>(): intptr;

declare function sizeof<byte>()   : 1;
declare function sizeof<u8>()     : 1;
declare function sizeof<sbyte>()  : 1;
declare function sizeof<s8>()     : 1;
declare function sizeof<short>()  : 2;
declare function sizeof<s16>()    : 2;
declare function sizeof<ushort>() : 2;
declare function sizeof<u16>()    : 2;
declare function sizeof<int>()    : 4;
declare function sizeof<s32>()    : 4;
declare function sizeof<uint>()   : 4;
declare function sizeof<u32>()    : 4;
declare function sizeof<long>()   : 8;
declare function sizeof<s64>()    : 8;
declare function sizeof<ulong>()  : 8;
declare function sizeof<u64>()    : 8;
declare function sizeof<float>()  : 4;
declare function sizeof<f32>()    : 4;
declare function sizeof<double>() : 8;
declare function sizeof<f64>()    : 8;
