module type GraphADT =
  sig
    type 'a graph
    val empty : unit -> 'a graph
    val is_empty : 'a graph -> bool
    val add_node : 'a graph -> 'a -> 'a graph
    val add_arc : 'a graph -> 'a -> 'a -> 'a graph
    val visit : 'a graph -> 'a list
    exception EmptyGraph
    exception NotInGraph
    exception InvalidGraph
  end;;

module Graph : GraphADT =
  struct
    exception EmptyGraph
    exception NotInGraph
    exception InvalidGraph

    type 'a graph = Graph of 'a list * (('a * 'a ) list)

    let empty() = Graph([],[])

    let is_empty g = (g=Graph([],[]))


    let is_in l elm =
      List.exists (fun a -> (a=elm)) l;;


    let add_node g n =
      match g with
        Graph([],[])->Graph([n],[])
        |Graph(nodes,arcs)->if (is_in nodes n) then Graph(nodes,arcs)
                            else Graph((n::nodes),arcs)

    let add_arc g start en =
      match g with
        Graph([],[])-> Graph((start::(en::[])), ((start,en)::[]))
        |Graph(nodes, arcs) -> if (is_in arcs (start,en)) then Graph(nodes, arcs)
                               else  match (add_node (add_node g start) en) with
                                       Graph(nodes, arcs)->Graph(nodes, ((start,en)::arcs))

    let adjiacents g a =
      match g with
        Graph([],[]) | Graph([],_) | Graph(_,[]) -> raise InvalidGraph
        |Graph(nodes,arcs)->
          List.map (fun x -> (match x with (_,b) -> b)) (List.filter (fun x -> (match x with (t,_) when t = a ->true |_->false)) arcs)

    let visit g =
      match g with
        Graph([],[])->raise EmptyGraph
        |Graph(nodes, arcs)-> let rec visit l visited =
                                match l with
                                  h::t when not(is_in visited h) -> visit t (visit (adjiacents g h) (h::visited))
                                  |h::t -> visit t visited
                                  |[]->visited
                              in visit nodes [];;


  end;;

(*
#use "social.ml";;
open Graph;;
let a = empty();;
let a = add_arc a "Frizzi" "Pallazzi";;
let a = add_arc a "Frizzi" "Gastani";;
let a = add_arc a "Gastani" "Sinchiurli";;
let a = add_arc a "Sinchiurli" "Schifarteruft";;
visit a;;
  *)
