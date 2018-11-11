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
        assert(LogicGenerator.bools_to_int(Array(false, true)) == 2)
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
        assert(LogicGenerator.bools_to_int(Array(false, false, true)) == 4)
      }
      
      it("(true, false, false)"){
        assert(LogicGenerator.bools_to_int(Array(true, false, false)) == 1)
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
  
  describe("int_to_rule_bools"){
    describe("size 1"){
      val size1: LogicGenerator = new LogicGenerator(1)
      it("返り値の長さチェック"){
        assert(size1.int_to_rule_bools(0).length == 2)
      }
      
      it("all false"){
        assert(size1.int_to_rule_bools(0).sameElements(Array(false, false)))
      }
      
      it("rule 1"){
        assert(size1.int_to_rule_bools(1).sameElements(Array(true, false)))
      }
    }
    
    describe("size 2"){
      val size2: LogicGenerator = new LogicGenerator(2)
      it("返り値の長さチェック"){
        assert(size2.int_to_rule_bools(0).length == 4)
      }
      
      it("all false"){
        assert(size2.int_to_rule_bools(0).sameElements(Array(false, false, false, false)))
      }
      
      it("先頭だけtrue"){
        assert(size2.int_to_rule_bools(1).sameElements(Array(true, false, false, false)))
      }
      
      it("2番目だけtrue"){
        assert(size2.int_to_rule_bools(2).sameElements(Array(false, true, false, false)))
      }
      
      it("最後だけtrue"){
        assert(size2.int_to_rule_bools(8).sameElements(Array(false, false, false, true)))
      }
      
      it("all true"){
        assert(size2.int_to_rule_bools(15).sameElements(Array(true, true, true, true)))
      }
    }
  }
}
