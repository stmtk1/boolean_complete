import org.scalatest.FunSpec

class IntBoolsConverterTest extends FunSpec {
  describe("bools_to_int"){ 
    describe("長さ２"){
      it("all true"){
        assert(Prover.bools_to_int(Array(true, true)) == 3)
      }
      
      it("(false, true)"){
        assert(Prover.bools_to_int(Array(false, true)) == 2)
      }
      
      it("all false"){
        assert(Prover.bools_to_int(Array(false, false)) == 0)
      }
    }
    describe("長さ３"){
      it("all false"){
        assert(Prover.bools_to_int(Array(false, false, false)) == 0)
      }
      
      it("(false, false, true)"){
        assert(Prover.bools_to_int(Array(false, false, true)) == 4)
      }
      
      it("(true, false, false)"){
        assert(Prover.bools_to_int(Array(true, false, false)) == 1)
      }
      
      it("(false, true, false)"){
        assert(Prover.bools_to_int(Array(false, true, false)) == 2)
      }
      
      it("all true"){
        assert(Prover.bools_to_int(Array(true, true, true)) == 7)
      }
    }
    
    describe("長さ４"){
      it("all false"){
        assert(Prover.bools_to_int(Array(false, false, false, false)) == 0)
      }
      
      it("all true"){
        assert(Prover.bools_to_int(Array(true, true, true, true)) == 15)
      }
    }
  }
  
  describe("int_to_bools"){
    describe("size 1"){
      val size: Int = 1
      it("返り値の長さチェック"){
        assert(Prover.int_to_bools(0, size).length == 1)
      }
      
      it("one false"){
        assert(Prover.int_to_bools(0, size).sameElements(Array(false)))
      }
      
      it("one true"){
        assert(Prover.int_to_bools(1, size).sameElements(Array(true)))
      }
    }
    
    describe("size 2"){
      val size: Int = 2 
      it("返り値の長さチェック"){
        assert(Prover.int_to_bools(0, size).length == 2)
      }
      
      it("all false"){
        assert(Prover.int_to_bools(0, size).sameElements(Array(false, false)))
      }
      
      it("rule 1"){
        assert(Prover.int_to_bools(1, size).sameElements(Array(true, false)))
      }
      
      it("all true"){
        assert(Prover.int_to_bools(3, size).sameElements(Array(true, true)))
      }
    }
    
    describe("size 3"){
      val size: Int = 3 
      it("返り値の長さチェック"){
        assert(Prover.int_to_bools(0, size).length == 3)
      }
      
      it("all false"){
        assert(Prover.int_to_bools(0, size).sameElements(Array(false, false, false)))
      }
      
      it("最初だけtrue"){
        assert(Prover.int_to_bools(1, size).sameElements(Array(true, false, false)))
      }
      
      it("真ん中だけtrue"){
        assert(Prover.int_to_bools(2, size).sameElements(Array(false, true, false)))
      }
      
      it("最後だけtrue"){
        assert(Prover.int_to_bools(4, size).sameElements(Array(false, false, true)))
      }
      
      it("all true"){
        assert(Prover.int_to_bools(7, size).sameElements(Array(true, true, true)))
      }
    }
    
    describe("size 4"){
      val size: Int = 4 
      it("返り値の長さチェック"){
        assert(Prover.int_to_bools(0, size).length == 4)
      }
      
      it("all false"){
        assert(Prover.int_to_bools(0, size).sameElements(Array(false, false, false, false)))
      }
      
      it("先頭だけtrue"){
        assert(Prover.int_to_bools(1, size).sameElements(Array(true, false, false, false)))
      }
      
      it("2番目だけtrue"){
        assert(Prover.int_to_bools(2, size).sameElements(Array(false, true, false, false)))
      }
      
      it("最後だけtrue"){
        assert(Prover.int_to_bools(8, size).sameElements(Array(false, false, false, true)))
      }
      
      it("all true"){
        assert(Prover.int_to_bools(15, size).sameElements(Array(true, true, true, true)))
      }
    }
  }

  describe("Int -> Int 結合テスト"){
    describe("0 -> 0"){
      it("size 1"){
        assert(Prover.bools_to_int((Prover.int_to_bools(0, 1))) == 0)
      }
      
      it("size 2"){
        assert(Prover.bools_to_int((Prover.int_to_bools(0, 2))) == 0)
      }
      
      it("size 3"){
        assert(Prover.bools_to_int((Prover.int_to_bools(0, 3))) == 0)
      }
      
      it("size 4"){
        assert(Prover.bools_to_int((Prover.int_to_bools(0, 4))) == 0)
      }
    }
    
    describe("1 -> 1"){
      it("size 1"){
        assert(Prover.bools_to_int((Prover.int_to_bools(1, 1))) == 1)
      }
      
      it("size 2"){
        assert(Prover.bools_to_int((Prover.int_to_bools(1, 2))) == 1)
      }
      
      it("size 3"){
        assert(Prover.bools_to_int((Prover.int_to_bools(1, 3))) == 1)
      }
      
      it("size 4"){
        assert(Prover.bools_to_int((Prover.int_to_bools(1, 4))) == 1)
      }
    }

    describe("all true"){
      it("size 2"){
        assert(Prover.bools_to_int((Prover.int_to_bools(3, 2))) == 3)
      }
      
      it("size 3"){
        assert(Prover.bools_to_int((Prover.int_to_bools(7, 3))) == 7)
      }
      
      it("size 4"){
        assert(Prover.bools_to_int((Prover.int_to_bools(15, 4))) == 15)
      }
    }
  }
  describe("Bools -> Bools 結合テスト"){
    describe("all false"){
      it("size 1"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false))), 1).sameElements(Array(false)))
      }
      
      it("size 2"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, false))), 2).sameElements(Array(false, false)))
      }
      
      it("size 3"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, false, false))), 3).sameElements(Array(false, false, false)))
      }
    }
    
    describe("all true"){
      it("size 1"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true))), 1).sameElements(Array(true)))
      }
      
      it("size 2"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, true))), 2).sameElements(Array(true, true)))
      }
      
      it("size 3"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, true, true))), 3).sameElements(Array(true, true, true)))
      }
    }
    
    describe("先頭だけtrue"){
      it("size 2"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, false))), 2).sameElements(Array(true, false)))
      }
      
      it("size 3"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, false, false))), 3).sameElements(Array(true, false, false)))
      }
      
      it("size 4"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, false, false, false))), 4).sameElements(Array(true, false, false, false)))
      }
    }
    
    describe("２番目だけtrue"){
      it("size 3"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, true, false))), 3).sameElements(Array(false, true, false)))
      }
      
      it("size 4"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, true, false, false))), 4).sameElements(Array(false, true, false, false)))
      }
      
      it("size 5"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, true, false, false, false))), 5).sameElements(Array(false, true, false, false, false)))
      }
    }
    
    describe("先頭だけfalse"){
      it("size 2"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, true))), 2).sameElements(Array(false, true)))
      }
      
      it("size 3"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, true, true))), 3).sameElements(Array(false, true, true)))
      }
      
      it("size 4"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(false, true, true, true))), 4).sameElements(Array(false, true, true, true)))
      }
    }
    
    describe("最後だけfalse"){
      it("size 3"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, true, false))), 3).sameElements(Array(true, true, false)))
      }
      
      it("size 4"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, true, true, false))), 4).sameElements(Array(true, true, true, false)))
      }
    }
    
    describe("２番目だけfalse"){
      it("size 3"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, false, true))), 3).sameElements(Array(true, false, true)))
      }
      
      it("size 4"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, false, true, true))), 4).sameElements(Array(true, false, true, true)))
      }
      
      it("size 5"){
        assert(Prover.int_to_bools((Prover.bools_to_int(Array(true, false, true, true, true))), 5).sameElements(Array(true, false, true, true, true)))
      }
    }
  }
}
