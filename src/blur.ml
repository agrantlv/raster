open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  let height = Image.height image in
  let width = Image.width image in
  Image.mapi image ~f:(fun ~x ~y _pixel ->
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
    Image.mean_pixel
      (Image.slice
         image
         ~x_start:(x - new_x_min_radius)
         ~x_end:(x + new_x_max_radius)
         ~y_start:(y - new_y_min_radius)
         ~y_end:(y + new_y_max_radius)))
;;

let%expect_test "test blur" =
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
    (* Image.load_ppm ~filename:"../images/beach_portrait.ppm" *)
  in
  let og_image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let new_image = transform og_image ~radius:3 in
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
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
