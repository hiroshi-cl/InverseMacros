package inverse_macros

//無限ループを避けるため inverse_macros はオリジナルの macro paradise でコンパイルされるのでモジュールを分けておく

object BasicTransformationTest {

  def correct1() = {
    val _1 = (): Unit@test1
    @IMSynth val _2 = (): Unit@test1
    (): Unit@test1
    ()
  }

  def correct2() = {
    val _1 = (): Unit@test1
    @IMSynth val _2 = ()
    ()
    (): Unit@test1
  }

  def correct3() = {
    def _1() = (): Unit@test1
    class _2 {
      val _3 = (): Unit@test1
    }
    (): Unit@test1
  }

  def correct4() =
    (10: Int@test1).toByte // Select

  def correct5() =
    if (true: Boolean@test1) ()

  def correct6() =
    if (true) 10: Int@test1 else 20

//  def error6() =
//    if (true: Boolean@test1) (): Unit@test1 else (): Unit@test2

  def correct7() = {
    lazy val hoge: Int = ??? : Int@test1
    hoge
  }

//  def error7() = {
//    lazy val hoge = ??? : Int@test1
//    hoge
//  }

//  def error8() = {
//    def error1(): Unit = (): Unit@test1
//    error1()
//  }

  def correct9(a: => Int) =
    if (true) 10: Int@test1 else a

//  def error9(a: => Int@test2) =
//    if (true) 10: Int@test1 else a

  // reflection によるテストは困難
}
