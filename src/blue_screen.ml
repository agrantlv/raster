open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    match pixel with
    | r, g, b -> if b > r + g then Image.get background ~x ~y else pixel)
;;

let _transform_improved ~foreground ~background =
  let blue_threshold = 35 * Image.max_val foreground / 100 in
  let non_blue_threshold = 52 * Image.max_val foreground / 100 in
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    match pixel with
    | r, g, b ->
      if r < non_blue_threshold
         && g < non_blue_threshold
         && b > blue_threshold
      then Image.get background ~x ~y
      else pixel)
;;

let transform_improved_2 ~foreground ~background =
  let radius = 5 in
  let height = Image.height foreground in
  let width = Image.width foreground in
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    let new_x_min_radius = if x < radius then x else radius in
    let new_y_min_radius = if y < radius then y else radius in
    let new_x_max_radius =
      if x + radius > width - 1 then width - 1 - x else radius
      (* if x + radius > width then width - x else radius *)
    in
    let new_y_max_radius =
      if y + radius > height - 1 then height - 1 - y else radius
      (* if y + radius > height then height - y else radius *)
    in
    let mean =
      Image.mean_pixel
        (Image.slice
           foreground
           ~x_start:(x - new_x_min_radius)
           ~x_end:(x + new_x_max_radius)
           ~y_start:(y - new_y_min_radius)
           ~y_end:(y + new_y_max_radius))
    in
    (* Pretty good settings: let blue_threshold = 40 * Image.max_val
       foreground / 100 in let non_blue_threshold = 35 * Image.max_val
       foreground / 100 in *)
    (* best settings*)
    let blue_threshold = 40 * Image.max_val foreground / 100 in
    let non_blue_threshold = 44 * Image.max_val foreground / 100 in
    match mean with
    | r, g, b ->
      if b > r + g
      then Image.get background ~x ~y
      else (
        match pixel with
        | pixel_r, pixel_g, pixel_b ->
          if pixel_r < non_blue_threshold
             && pixel_g < non_blue_threshold
             && pixel_b > blue_threshold
          then Image.get background ~x ~y
          else pixel (* pixel *)))
;;

let%expect_test "test blue_screen" =
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
    (* Image.load_ppm ~filename:"../images/beach_portrait.ppm" *)
  in
  let og_image_background =
    Image.load_ppm ~filename:"../images/meadow.ppm"
  in
  let og_image_foreground =
    Image.load_ppm ~filename:"../images/oz_bluescreen.ppm"
  in
  let new_image =
    transform ~foreground:og_image_foreground ~background:og_image_background
  in
  let _ =
    Image.mapi new_image ~f:(fun ~x ~y pixel ->
      if not (Pixel.equal pixel (Image.get reference_image ~x ~y))
      then print_s [%message (x : int) (y : int)];
      pixel)
  in
  [%expect ""]
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform_improved_2 ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
