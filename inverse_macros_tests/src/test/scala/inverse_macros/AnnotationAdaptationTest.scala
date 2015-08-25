package inverse_macros

//無限ループを避けるため inverse_macros はオリジナルの macro paradise でコンパイルされるのでモジュールを分けておく

object AnnotationAdaptationTest {
//    def error1(): Unit = (): Unit@test1 // adaptation problem
//
//    def error2(): Unit@test1 = (): Unit@test1ex
//
//    def error3(): Unit@test2gen[String] = (): Unit@test2gen[Any]
//
//    def error4(): Unit@test1 @test2gen[Any] = (): Unit
//
//    def error5(): Unit = (): Unit@test1 @test2gen[Any]
//
//    def error6(): Unit@test1 @test2gen[Any] = (): Unit@test1

  def correct1(): Unit@test1 = (): Unit@test1

  def correct2() = (): Unit@test1 // no adaptation

  def correct3(): Unit@test1 = () // no transform, no adaptation

  def correct4() = (): Unit@unchecked // non-related annotation

  def correct5(): Unit = {
    correct1() // Unit@IMAnnotation
    ()
  }

  def correct6(): Unit@test2gen[Any] = (): Unit@test2gen[String]

  def correct7(): Unit@test1 = (): Unit@test1 @unchecked

  def correct8(): Unit@unchecked @unchecked = ()

  // reflection によるテストは困難
}
