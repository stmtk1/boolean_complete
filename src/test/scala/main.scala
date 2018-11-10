import org.scalatest.FunSpec

class LogicGeneratorTest extends FunSpec {
  //with DiagrammedAssertions {
  describe("コンストラクタ") {
    describe("引数") {
      it("size"){
        val lg : LogicGenerator = new LogicGenerator(3)
        assert(lg.size == 3)
      }
    }
  }
  describe("bools_to_int"){ 
    describe("長さ２"){
      it("all true"){
        assert(LogicGenerator.bools_to_int(Array(true, true)) == 3)
      }
      
      it("(false, true)"){
        assert(LogicGenerator.bools_to_int(Array(false, true)) == 1)
      }
      
      it("all false"){
        assert(LogicGenerator.bools_to_int(Array(false, false)) == 0)
      }
    }
    describe("長さ３"){
      it("all false"){
        assert(LogicGenerator.bools_to_int(Array(false, false, false)) == 0)
      }
      
      it("(false, false, true)"){
        assert(LogicGenerator.bools_to_int(Array(false, false, true)) == 1)
      }
      
      it("(true, false, false)"){
        assert(LogicGenerator.bools_to_int(Array(true, false, false)) == 4)
      }
      
      it("(false, true, false)"){
        assert(LogicGenerator.bools_to_int(Array(false, true, false)) == 2)
      }
      
      it("all true"){
        assert(LogicGenerator.bools_to_int(Array(true, true, true)) == 7)
      }
    }
    
    describe("長さ４"){
      it("all false"){
        assert(LogicGenerator.bools_to_int(Array(false, false, false, false)) == 0)
      }
      
      it("all true"){
        assert(LogicGenerator.bools_to_int(Array(true, true, true, true)) == 15)
      }
    }
  }
}
