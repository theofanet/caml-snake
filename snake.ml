type direction = UP | DOWN | LEFT | RIGHT

let score = ref 0
let taille_plateau = ref 20
let pomme = ref (5, 5)
let max = ref 0

let _ = 
  let ic = open_in "./score" in
  try 
    let a = input_line ic in
    max := int_of_string a
  with End_of_file -> close_in ic

let save_score_max () =
  let oc = open_out "./score" in
  output_string oc (string_of_int !max);
  close_out oc

let with_cbreak (f,s) =
  let term_init = Unix.tcgetattr Unix.stdin in
  let term_cbreak = { term_init with Unix.c_icanon = false } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW term_cbreak;
  try
    let result = f s in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term_init;
    result
  with e ->
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term_init;
    raise e

let () = ignore (Sys.command "clear")

let handle_keyboard s = 
  while true do
    let char = Char.code (input_char stdin) in
    match char with
    | 65 -> if s#dir <> UP then s#set_dir DOWN
    | 66 -> if s#dir <> DOWN then s#set_dir UP
    | 67 -> if s#dir <> LEFT then s#set_dir RIGHT
    | 68 -> if s#dir <> RIGHT then s#set_dir LEFT
    | 32 -> print_string "\n";exit 0
    | _  -> ()
(*| k  -> Printf.printf "touch : %d\n" k*)
  done

let wait m =
  let sec = m /. 1000. in
  let tm1 = Unix.gettimeofday () in
  while Unix.gettimeofday () -. tm1 < sec do
    ()
  done

let clear = 
  try
    let proc = Unix.open_process_in "clear" in
    try
      let chars = input_line proc in
      ignore (Unix.close_process_in proc);
      chars
    with e -> ignore (Unix.close_process_in proc); ""
  with _ -> ""

class snake taille = object (self)
  val mutable length : int = taille
  val mutable corps = Array.make taille (0, 0)
  val mutable dir = UP
  method dir = dir
  method set_dir d = dir <- d
  method game_over = 
    begin
      Printf.printf "\nGAME OVER !\n";
      flush stdout;
      save_score_max ();
      exit 0
    end
  method add_taille map = 
    begin
      score := !score+1;
      pomme := (Random.int(Array.length map.(0)-1), Random.int(Array.length map - 1));
      let new_array = Array.create (length+1) (0, 0) in
      let (lx, ly)  = corps.(Array.length corps-1) in
      for i = 0 to Array.length corps - 1 do
	new_array.(i) <- corps.(i)
      done;
      new_array.(Array.length new_array - 1) <- (lx, ly);
      length <- length+1;
      corps <- new_array;
      if !score > !max then max := !score
    end
  method update_reste = 
    begin
      for i = Array.length corps - 1 downto 1 do
	corps.(i) <- corps.(i - 1)
      done
    end
  method update map = 
    begin
      if map.(0).(0) = " " || map.(0).(0) = "X" || map.(0).(0) = "O" then 
      match dir with 
	| UP    -> let (x, y) = corps.(0) in if y+1 > Array.length map - 1 then corps.(0) <- (x, 0) else corps.(0) <- (x, y+1);self#update_reste
	| DOWN  -> let (x, y) = corps.(0) in if y-1 < 0 then corps.(0) <- (x, (Array.length map - 1)) else corps.(0) <- (x, y-1);self#update_reste
	| LEFT  -> let (x, y) = corps.(0) in if x-1 < 0 then corps.(0) <- ((Array.length map.(0) - 1), y) else corps.(0) <- (x-1, y);self#update_reste
	| RIGHT -> let (x, y) = corps.(0) in if x+1 > Array.length map.(0) - 1 then corps.(0) <- (0, y) else corps.(0) <- (x+1, y);self#update_reste
    end
  method check_collision map = 
    begin
      self#check_body_collision;
      self#check_pomme_collision map;
    end
  method check_body_collision =
    begin
      let (sx, sy) = corps.(0) in
      for i = 2 to Array.length corps - 2 do
	let (cx, cy) = corps.(i) in
	if cx = sx && cy = sy then
	  self#game_over
      done
    end
  method check_pomme_collision map =
    begin
      let (px, py) = !pomme in
      let (sx, sy) = corps.(0) in
      if px = sx && py = sy && (map.(0).(0) = " " || map.(0).(0) = "X" || map.(0).(0) = "O") then
	self#add_taille map
    end
  method draw map =
    begin
      let (px, py) = !pomme in
      print_string clear;
      for i = 0 to Array.length map - 1 do
	for j = 0 to Array.length map.(i) - 1 do
	  if px = j && py = i then map.(i).(j) <- "O" else map.(i).(j) <- " "
	done
      done;
      for i = 0 to Array.length corps - 1 do
	let (x, y) = corps.(i) in map.(y).(x) <- "X"
      done;
      for i = 0 to Array.length map.(0) + 1 do
	print_string "#"
      done;
      print_string "\n";
      for i = 0 to Array.length map - 1 do
	print_string "#";
	for j = 0 to Array.length map.(i) - 1 do
	  print_string map.(i).(j)
	done;
	  flush stdout;
	  print_string "#\n"
      done;
      for i = 0 to Array.length map.(0) + 1 do
	print_string "#"
      done;
      Printf.printf "\nScore Max : %i\nScore : %i\n" !max !score;
      flush stdout
    end
end
      
let main (map,s) = 
  begin
    while true do
      s#update map;
      s#draw map;
      s#check_collision map;
      if s#dir = UP || s#dir = DOWN then 
	wait 60.
      else
	wait 40.
    done
  end

let _ = 
  let _ = if Array.length Sys.argv = 2 then
            taille_plateau := int_of_string Sys.argv.(1) 
  in
  let _ = Random.init (int_of_float (Unix.gettimeofday ()))  in
  let s = new snake 1 in
  let map = Array.make_matrix !taille_plateau (!taille_plateau*2) " " in
  let t0 = Thread.create with_cbreak (handle_keyboard, s) in
  let t1 = Thread.create main (map, s) in
  Thread.join t0;
  Thread.join t1
