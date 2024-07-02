open! Core

let solarize image : Image.t =
  let max_val = Image.max_val image in
  let threshold = 4 * max_val / 10 in
  Image.map image ~f:(fun pixel ->
    let r = Pixel.red pixel in
    let g = Pixel.green pixel in
    let b = Pixel.blue pixel in
    let new_r = if r > threshold then max_val - r else r in
    let new_g = if g > threshold then max_val - g else g in
    let new_b = if b > threshold then max_val - b else b in
    new_r, new_g, new_b)
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        (* let image = Image.load_ppm ~filename |> transform in *)
        let image = Image.load_ppm ~filename |> solarize in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
