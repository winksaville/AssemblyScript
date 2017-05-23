/** An 8-bit unsigned integer. */
declare type byte = number;

/** An 8-bit signed integer. */
declare type sbyte = number;

/** A 16-bit signed integer. */
declare type short = number;

/** A 16-bit unsigned integer. */
declare type ushort = number;

/** A 32-bit signed integer. */
declare type int = number;

/** A 32-bit unsigned integer. */
declare type uint = number;

/** A 64-bit signed integer. */
declare type long = number;

/** A 64-bit unsigned integer. */
declare type ulong = number;

/** A 32-bit float. */
declare type float = number;

/** A 64-bit float. */
declare type double = number;

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
declare function sizeof<sbyte>()  : 1;
declare function sizeof<short>()  : 2;
declare function sizeof<ushort>() : 2;
declare function sizeof<int>()    : 4;
declare function sizeof<uint>()   : 4;
declare function sizeof<long>()   : 8;
declare function sizeof<ulong>()  : 8;
declare function sizeof<float>()  : 4;
declare function sizeof<double>() : 8;
