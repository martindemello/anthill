module type ENV = sig
  type dict

  type env = {
    dict : dict;
    mutable op : Types.fn
  }
end

module Make (Dict : sig type t end) = struct
  type dict = Dict.t

  type env = {
    dict : dict;
    mutable op : Types.fn
  }
end
