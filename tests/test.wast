(module
 (type $iii (func (param i32 i32) (result i32)))
 (type $iv (func (param i32)))
 (type $v (func))
 (type $iFi (func (param i32 f64) (result i32)))
 (memory $0 256)
 (export "memory" (memory $0))
 (export "add" (func $add))
 (start $start)
 (func $Hello$world (type $iii) (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (call $Hello$anotherVoid
   (get_local $0)
  )
  (call $Hello$anotherStaticVoid)
  (set_local $2
   (i32.const 1)
  )
  (loop $break$0
   (block $continue$0
    (br_if $break$0
     (i32.eqz
      (get_local $2)
     )
    )
    (return
     (get_local $2)
    )
   )
  )
 )
 (func $Hello$anotherVoid (type $iv) (param $0 i32)
 )
 (func $Hello$anotherStaticVoid (type $v)
 )
 (func $add (type $iFi) (param $0 i32) (param $1 f64) (result i32)
  (return
   (i32.shl
    (i32.shr_s
     (i32.add
      (get_local $0)
      (i32.trunc_s/f64
       (get_local $1)
      )
     )
     (i32.const 16)
    )
    (i32.const 16)
   )
  )
 )
 (func $start (type $v)
 )
)
