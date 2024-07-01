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
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
