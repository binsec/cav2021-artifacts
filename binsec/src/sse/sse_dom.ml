open Sse_graph

module Dom = Graph.Dominator.Make(struct
    include G

    let pred = G.succ
    let succ = G.pred
    let iter_succ = G.iter_pred
  end)

let compute_merge_point from_address depth =
  let g = G.create 13 in
  populate_from ~skip_calls:true g (Dba_types.Caddress.to_virtual_address from_address) depth;
  let entry = G.V.of_addr from_address in
  (* a dummy exit node *)
  let exit = Dba_types.Caddress.block_start_of_int 0 |> G.V.of_addr in
  G.add_vertex g exit;
  G.iter_vertex (fun v ->
      if G.out_degree g v = 0 then
        G.add_edge g v exit
    ) g;
  Sse_options.Logger.debug ~level:10 "merge graph = %a" pp_as_dot g;
  let dom = Dom.compute_idom g exit entry in
  if G.V.equal dom exit
  then None
  else Some(G.V.addr dom)
