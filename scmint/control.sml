structure Control: CONTROL = 
  struct 
  open Error Object

  fun apply (p, x, l) =
      let fun args nil = raise Impossible "apply"
            | args [a] = LIST_UNTAG a
            | args (a::b) = a :: args b
      in p (args (x::l))
      end
  local
  val mapc = map
  fun list2listoflists nil = nil
    | list2listoflists (a::b) = [a] :: list2listoflists b
  in 
  fun map (p, l, nil) =  mapc p (list2listoflists l)
    | map _ = raise Unimplemented "map"
  fun for_each (p, l, nil) = app p (list2listoflists l)
    | for_each _ = raise Unimplemented "for-each"
  end
  local
  fun callcc _ = raise Unimplemented "callcc"
  fun throw _ = raise Unimplemented "throw"
  in
  fun call_with_current_continuation p = callcc (p o throw) 
  end

  end
