open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image : Image.t =
  Image.map image ~f:(fun pixel ->
    match pixel with
    | r, g, b ->
      let avg_val = (r + g + b) / 3 in
      avg_val, avg_val, avg_val)
;;

let%expect_test "test grayscale" =
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
    (* Image.load_ppm ~filename:"../images/beach_portrait.ppm" *)
  in
  let og_image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let new_image = transform og_image in
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
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
