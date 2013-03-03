type t = {
  mutable x: float ;
  mutable y: float ;
  mutable width: float ;
  mutable height: float
}

let isInRect (px,py) geom = 
    (geom.x < px 
     && geom.y < py 
     && geom.x +. geom.width > px
     && geom.y +. geom.height > py)
      
let resize geom (w,h) =
  geom.width <- w ;
  geom.height <- h

let size geom =
  (geom.width,geom.height)


let move geom (x,y) =
  geom.x <- x ;
  geom.y <- y

let position geom =
  (geom.x, geom.y)

let create (x,y) (width,height) =
  { x ; y ; width ; height }

let copy geom =
   create (position geom) (size geom)

