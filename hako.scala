import scala.collection.mutable

object Hako{
  type Mtrx = Array[Array[Option[Block]]]

  sealed abstract class Dir
  object Dir {
    case object Left extends Dir
    case object Right extends Dir
    case object Up extends Dir
    case object Down extends Dir
    val all = List(Left, Right, Up, Down)
  }

  class Block(
    val id:Int, val x:Int, val y:Int, val w:Int, val h:Int, val role:Char
  ) {

    val all_pos = for(x2<-0 until w; y2<-0 until h) yield(x+x2,y+y2)

    def move(dir:Dir):Block = {
      dir match {
        case Dir.Left => new Block(id,x-1,y,w,h,role)
        case Dir.Right => new Block(id,x+1,y,w,h,role)
        case Dir.Up => new Block(id,x,y-1,w,h,role)
        case Dir.Down => new Block(id,x,y+1,w,h,role)
      }
    }

    private val tpl = (x,y,w,h)
    private val hash = tpl.hashCode()
    override def hashCode = hash

    override def equals(other:Any) = other match {
      case that: Block => tpl == that.tpl
      case _ => false
    }
  }

  class Board(val w:Int, val h:Int, val mtrx:Mtrx, val blocks:Set[Block]) {
    private def filled_mtrx(old_blc:Block, new_blc:Block) = {
      var ary = mtrx.map(_.clone)
      for ((x, y) <- old_blc.all_pos) ary(y)(x) = None
      for ((x, y) <- new_blc.all_pos) ary(y)(x) = Some(new_blc)
      ary
    }

    def move(block:Block, dir:Dir): Option[Board] = {
      def movable_x(dst_x:Int)= {
        dst_x >= 0 &&
        dst_x < w &&
          (0 until block.h).forall(y2 => mtrx(block.y+y2)(dst_x).isEmpty)
      }
      def movable_y(dst_y:Int) = {
        dst_y >= 0 &&
        dst_y < h &&
          (0 until block.w).forall(x2 => mtrx(dst_y)(block.x+x2).isEmpty)
      }

      val valid = dir match {
        case Dir.Left => movable_x(block.x-1)
        case Dir.Right => movable_x(block.x+block.w)
        case Dir.Up => movable_y(block.y-1)
        case Dir.Down => movable_y(block.y+block.h)
      }
      if (!valid) return None

      val new_blc = block.move(dir)
      val new_ary = filled_mtrx(block, new_blc)
      Some(new Board(w, h, new_ary, blocks-block+new_blc))
    }

    def role(x:Int, y:Int):Char = mtrx(y)(x) match {
      case Some(blc) => blc.role
      case None => '_'
    }

    private val hash = blocks.hashCode()
    override def hashCode = hash

    override def equals(other:Any) = other match {
      case that: Board => blocks == that.blocks
      case _ => false
    }
  }

  object Solver{
    class Step(val prev:Option[Step], val x:Int, val y:Int, val role:Char, val dir:Option[Dir], val n:Int)
    class Frame(val board:Board, val step: Step)

    def solve(board:Board)(cond:Board => Boolean): Option[Frame] = {
      val queue = new mutable.Queue[Frame]
      val cache = new mutable.HashMap[Board, Step]
      queue += new Frame(board, new Step(None, 0, 0, '~', None, 0))

      while(!queue.isEmpty){
        val frame = queue.dequeue
        val step = cache.get(frame.board)
        if (step.isEmpty || step.get.n > frame.step.n){
          cache(frame.board) = frame.step

          for (block <- frame.board.blocks; dir <- Dir.all){
            frame.board.move(block, dir) match {
              case Some(bd) =>
                val st = new Step(Some(frame.step), block.x, block.y, block.role, Some(dir), frame.step.n+1)
                val fr = new Frame(bd, st)
                if (cond(bd)) return Some(fr)

                queue += fr
              case None =>
            }
          }
        }
      }

      None
    }
  }

  def solve(){
    def make_board() = {
      var id = 0;

      def set(ary:Mtrx, x:Int, y:Int, w:Int, h:Int, role:Char){
        val block = new Block(id, x, y, w, h, role)
        id += 1;
        for ((x, y) <- block.all_pos) ary(y)(x) = Some(block)
      }

      val (w, h) = (4, 5)
      var ary = Array.fill[Option[Block]](h,w){None}
      set(ary, 0,0,1,2,'T')
      set(ary, 1,0,2,2,'H')
      set(ary, 3,0,1,2,'T')

      set(ary, 0,2,1,2,'T')
      set(ary, 1,2,2,1,'W')

      set(ary, 1,3,1,1,'S')
      set(ary, 2,3,1,1,'S')
      set(ary, 3,2,1,2,'T')

      set(ary, 0,4,1,1,'S')
      set(ary, 3,4,1,1,'S')

      val blocks = (ary.flatten.toSet-None).map(_.get)
      new Board(w,h,ary,blocks)
    }

    println("start")

    val board = make_board()
    val res = Solver.solve(board){b => b.role(1,4)=='H' && b.role(2,4)=='H'}
    res match {
      case Some(frame) =>
        var step = frame.step
        while (step.prev.isDefined){
          println(step.x, step.y, step.role, step.dir.get)
          step = step.prev.get
        }

        println(frame.step.n)
      case None =>
        println("none")
    }
  }
}

object Main {
  def main(args: Array[String]) {
    Hako.solve()
  }
}
