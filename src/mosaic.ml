open Core

let find_mse img1 img2 ~width ~height =
  let mse_sum : int =
    Image.foldi img1 ~init:0 ~f:(fun ~x ~y sum pixel1 ->
      let pixel2 = Image.get img2 ~x ~y in
      let pixel_diff =
        Pixel.red pixel1
        - Pixel.red pixel2
        + (Pixel.green pixel1 - Pixel.green pixel2)
        + (Pixel.blue pixel1 - Pixel.blue pixel2)
      in
      Int.pow pixel_diff 2 + sum)
  in
  mse_sum / (width * height)
;;

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let rec transform image ~mos_width ~mos_height ~num_moves =
  if num_moves = 0
  then image
  else (
    let img_height = Image.height image in
    let img_width = Image.width image in
    let start_tuple = Image.slice, find_mse in
    let similar_tuple : Image.t * int =
      Image.foldi image ~init:() ~f:(fun ~x ~y _pixel ->
        let new_x_min_radius = if x < mos_width then x else mos_width in
        let new_y_min_radius = if y < mos_height then y else mos_height in
        let new_x_max_radius =
          if x + mos_width > img_width - 1
          then img_width - 1 - x
          else mos_width
          (* if x + radius > width then width - x else radius *)
        in
        let new_y_max_radius =
          if y + mos_height > img_height - 1
          then img_height - 1 - y
          else mos_height
          (* if y + radius > height then height - y else radius *)
        in
        Image.mean_pixel
          (Image.slice
             image
             ~x_start:(x - new_x_min_radius)
             ~x_end:(x + new_x_max_radius)
             ~y_start:(y - new_y_min_radius)
             ~y_end:(y + new_y_max_radius)))
    in
    transform new_img ~mos_width ~mos_height ~num_moves:(num_moves - 1))
;;

(* let rec transform_rec image ~width ~height num_moves = if num_moves = 0
   then image else transform image ~width ~height num_moves - 1 ;; *)
(* let%expect_test "test blur" = let reference_image = Image.load_ppm
   ~filename:"../images/reference-beach_portrait_blur.ppm" (* Image.load_ppm
   ~filename:"../images/beach_portrait.ppm" *) in let og_image =
   Image.load_ppm ~filename:"../images/beach_portrait.ppm" in let new_image =
   transform og_image ~radius:3 in let _ = Image.mapi new_image ~f:(fun ~x ~y
   pixel -> if not (Pixel.equal pixel (Image.get reference_image ~x ~y)) then
   print_s [%message (x : int) (y : int)]; pixel) in [%expect ""] ;; *)

let command =
  Command.basic
    ~summary:"Mosaic an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
