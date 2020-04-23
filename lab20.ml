open Graphics ;;

class grayscale_image (image : float list list) =
  object 

  method image = image

  (* Generic Grayscale Helper Function *)
  method grayscale (img : float list list) (fn : float -> float) : float list list = 
    List.map (fun row -> List.map fn row) img

  (* Different Types of Grayscale Strategies *)
  method threshold (t : float) : ('a -> 'b) = fun v -> if v <= t then 0. else 1.
  method dither = fun v -> if v > Random.float 1. then 1. else 0.

  (* Define Render Images *)
  method depict img =
    Graphics.open_graph ""; Graphics.clear_graph ();
    let x, y = List.length (List.hd img), List.length img in Graphics.resize_window x y;
    let depict_pix v r c = let lvl = int_of_float (255. *. (1. -. v)) in Graphics.set_color (Graphics.rgb lvl lvl lvl);
    plot c (y - r) in
    List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c) row) img;
    Unix.sleep 2; Graphics.close_graph ()
      
  end

(* Render Images *)
let mona = new grayscale_image Monalisa.image ;;

mona#depict mona#image ;;
mona#depict (mona#grayscale mona#image (mona#threshold 0.75)) ;;
mona#depict (mona#grayscale mona#image mona#dither) ;;
           