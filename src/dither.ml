open Core

let set_image ~image ~x ~y ~error_val =
  Image.set
    image
    ~x
    ~y
    (Pixel.( + ) (Image.get image ~x ~y) (Pixel.of_int error_val))
;;

(* This should look familiar by now! *)
let transform image =
  let image = Grayscale.transform image in
  let _ =
    Image.mapi image ~f:(fun ~x ~y pixel ->
      let max_val = Image.max_val image in
      let pixel_val = Pixel.red pixel in
      let new_pixel =
        if pixel_val > max_val / 2 then Pixel.of_int max_val else Pixel.zero
      in
      let error_val = pixel_val - Pixel.red new_pixel in
      Image.set image ~x ~y new_pixel;
      let _ =
        if x < Image.width image - 1
        then set_image ~image ~x:(x + 1) ~y ~error_val:(error_val * 7 / 16)
      in
      let _ =
        if x > 0 && y < Image.height image - 1
        then
          set_image
            ~image
            ~x:(x - 1)
            ~y:(y + 1)
            ~error_val:(error_val * 3 / 16)
      in
      let _ =
        if y < Image.height image - 1
        then set_image ~image ~x ~y:(y + 1) ~error_val:(error_val * 5 / 16)
      in
      let _ =
        if x < Image.width image - 1 && y < Image.height image - 1
        then
          set_image
            ~image
            ~x:(x + 1)
            ~y:(y + 1)
            ~error_val:(error_val * 1 / 16)
      in
      pixel)
  in
  image
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
