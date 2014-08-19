package easyparser

import java.text.SimpleDateFormat
import java.util.Date

import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class XmlPickerTest extends Specification {
  import play.api.libs.functional.syntax._
  import Reader.readerIsAnApplicative

  implicit val dtFormatter = new SimpleDateFormat("dd/MM/yyyy")
  import Converter.string2StringConverter
  import Converter.string2DateConverter

  val xml = """
  <company name="Dupont and Co">
    <owner>
      <person firstname="jean" lastname="dupont" birthdate="11/03/1987"/>
    </owner>
  </company>"""

  "A XML Picker" should {
    "be able to read XML content" in {
      val reader = (
        XmlPicker(_ \\ "person" \ "@firstname").as[String] and
          XmlPicker(_ \\ "person" \ "@lastname").as[String] and
          XmlPicker(_ \\ "person" \ "@birthdate").as[Date]
        )(Person)
      reader(xml) === Success(Person("jean", "dupont", dtFormatter.parse("11/03/1987")))
    }
  }
  case class Person(firstname: String, lastname: String, birthDate: Date)
}

