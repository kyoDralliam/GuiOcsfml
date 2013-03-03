type t = {
  mutable x: float ;
  mutable y: float ;
  mutable width: float ;
  mutable height: float
}

void isInRect (px,py) geom = 
    (geom.x < px 
     && geom.y < py 
     && geom.x +. geom.width > px
     && geom.y +. geom.height > py
