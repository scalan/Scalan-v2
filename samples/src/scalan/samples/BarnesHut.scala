package scalan.samples

import scalan.dsl._

//trait BarnesHut extends Scalan {
//  type Point = (Float, Float)             // x and y coordinates
//  type Vector = (Float, Float)            // 2d vector
//  type Force = Vector
//  type Centroid = (Float, Point)          // mass and position
//  type Particle = (Centroid, Vector)      // position and velocity
//  type Area = (Point, Point)              // lower left and upper right point of the area
//  type CentroidTree = Tree[Centroid]      // tree of centroids
//
////  implicit val pointElem: Elem[Point] = element[Point]
////  implicit val centroidElem: Elem[Centroid] = element[Centroid]
////  implicit val particleElem: Elem[Particle] = element[Particle]
////  implicit val treeElem: Elem[CentroidTree] = element[CentroidTree]
//
//  class PointW(val p: Point) {
//    def +(that: Point) = (p._1 + that._1, p._2 + that._2)
//    def -(that: Point) = (p._1 - that._1, p._2 - that._2)
//    def *(c: Float) = (p._1 * c, p._2 * c)
//    def /(c: Float) = (p._1 / c, p._2 / c)
//  }
//  implicit def pimpPoint(p: Point) = new PointW(p)
//
//  val timeStep: Float = 1
//
//  def splitArea(a: Area): PA[Area] = {
//    val ((l,b),(r,t)) = a
//    val cx = (r + l) / 2
//    val cy = (t + b) / 2
//    fromArray(Array(((l,b),(cx,cy)),((cx,b),(r,cy)),((cx,cy),(r,t)),((l,cy),(cx,t))))
//  }
//
//  def moveParticles(ps: PA[Particle], fs: PA[Force]): PA[Particle] = {
//    ps.zipWith (moveParticle) (fs)
//  }
//
//  def moveParticle(p:Particle)(force: Force): Particle = {
//    val ((m, loc),vel) = p
//    val accel = force / m
//    ((m, (loc + vel * timeStep)),vel + accel * timeStep)
//  }
//
//  def buildTree(area: Area, particles: PA[Particle]): CentroidTree = {
//    if (particles.length ==  1) {
//      val ((m, loc),vel) = particles(0)
//      Tree((m, loc), emptyArrayOf[CentroidTree])
//    }
//    else {
//      val subtrees = for (
//        a <- splitArea(area);
//        val ps = for (p <- particles if inArea(a,p)) yield p
//        if ps.length > 0)
//        yield (buildTree(a, ps))
//
//      val (m,l) = calcCentroid(subtrees)
//      Tree((m, l), subtrees)
//    }
//  }
//
//  def moveTree(tree: CentroidTree, vec: Vector): CentroidTree = {
//    val (m, loc) = tree.value
//    if (tree.children.length == 0) {
//      Tree((m, loc + vec), tree.children)
//    }
//    else {
//      val subtrees = for (subtree <- tree.children) yield (moveTree(subtree, vec))
//      Tree((m, loc + vec), subtrees)
//    }
//  }
//
//  def calcCentroid(ts: PA[CentroidTree]): Centroid = {
//    val (centroids, _) = unzipTree(ts)
//    val (masses, locations) = unzip(centroids)
//    val (xs,ys) = unzip(locations)
//    val len = locations.length
//    (sum(masses), (sum(xs)/len, sum(ys)/len))
//  }
//
//  def inArea(a: Area, p: Particle) = {
//    val ((lx,ly),(hx,hy)) = a
//    val ((_, (x,y)),_) = p
//    lx <= x && x <= hx && ly <= y && y <= hy
//  }
//}
