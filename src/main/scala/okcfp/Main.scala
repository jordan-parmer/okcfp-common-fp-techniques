package okcfp

import scala.collection.Iterator
import scala.util.Try
import scalaz._
import scalaz.Scalaz._

object Main {
  def main(args: Array[String]): Unit = {
    val emitRecords: () => Seq[RawUser] = Source.emit(Config.current)

    val result: Result = go(emitRecords)

    // side-effect print to see results
    println(result.toString)
  }

  private def go(emit: () => Seq[RawUser]): Result = {
    emit()
      .map(transform)
      .map(toResult)
      .foldLeft(Result.zero)(_ +: _)
  }

  private def toResult(v: \/[TransformError, DomainUser]): Result = {
    v match {
      case \/-(_) => Result(1, 0)
      case -\/(e) => {
        println(s"Transform Error: ${e.error}")
        Result(0, 1)
      }
    }
  }

  private def transform(r: RawUser): \/[TransformError, DomainUser] = {
    val maybePerson: TransformError \/ Person = r.person
    val maybePhone: TransformError \/ PhoneNumber = PhoneNumber.from(r.phone)

    (maybePerson |@| maybePhone)(DomainUser)
  }
}

object Source {
  def emit(conf: Config)(): Seq[RawUser] = RawData.generateRawUsers
}

trait Config
case class FileConfig() extends Config

object Config {
  def current: Config = FileConfig()
}

case class Result(successes: Int, failures: Int) {
  def +:(that: Result) =
    Result(
      this.successes + that.successes,
      this.failures + that.failures)
}

object Result {
  def zero = Result(0, 0)
}

case class TransformError(error: String)

case class DomainUser(
  person: Person,
  phoneNumber: PhoneNumber)

case class PhoneNumber(
    countryCode: Int,
    areaCode: Int,
    prefix: Int,
    lineNumber: Int) {
  override def toString = s"$countryCode ($areaCode) $prefix-$lineNumber"
}

object PhoneNumber {
  private val pattern = """(\d{1})-(\d{3})-(\d{3})-(\d{4})""".r
  private def toInt(s: String): TransformError \/ Int =
    Try(s.toInt).toDisjunction.leftMap(e => TransformError(e.getMessage))

  def from(phoneString: String): TransformError \/ PhoneNumber = {
    phoneString match {
      case pattern(code, area, prefix, line) =>
        (toInt(code) |@| toInt(area) |@| toInt(prefix) |@| toInt(line))(PhoneNumber.apply _)
        //\/-(PhoneNumber(code.toInt, area.toInt, prefix.toInt, line.toInt))
      case _ => -\/(TransformError(s"$phoneString didn't match expected phone number pattern"))
    }
  }
}

case class Person(firstName: String, lastName: String)

case class RawUser(
    fullName: String,
    email: String,
    phone: String,
    streetAddress: String,
    city: String,
    zipCode: String) {
  lazy val person: TransformError \/ Person = {
    fullName.split(" ").toList match {
      case first :: last :: Nil => \/-(Person(first, last))
      case _ => -\/(TransformError(s"Failed parsing first and last name from $fullName"))
    }
  }
}

object RawData {
  def generateRawUsers: Seq[RawUser] = Iterator(
    RawUser("Roth Drake", "vestibulum.nec@eratEtiam.net", "1-230-665-4456", "P.O. Box 980, 4942 Mattis. St.", "Gellik", "10691"),
    RawUser("Kevin Kaufman", "sem@necorciDonec.ca", "1-609-284-0788", "Ap #840-3698 Ipsum. Ave", "Ficarolo", "25265"),
    RawUser("Isaiah Yang", "Phasellus.libero.mauris@iaculisquispede.edu", "1-721-480-4797", "P.O. Box 701, 2269 Orci. Road", "Limelette", "22598"),
    RawUser("Hall Dale", "turpis@Cumsociis.net", "1-153-279-7425", "Ap #682-1860 Vivamus St.", "Oyen", "47919"),
    RawUser("Harlan Ferguson", "feugiat.nec.diam@ac.edu", "1-185-179-1491", "Ap #146-2358 Risus, Rd.", "Murdochville", "50832"),
    RawUser("Lance Cabrera", "non.sollicitudin@natoquepenatibuset.co.uk", "1-413-858-6331", "Ap #769-4638 In Avenue", "Arvier", "66909"),
    RawUser("Yuli Morrow", "nec.metus@loremvehiculaet.ca", "1-151-457-7940", "9847 Tempus Rd.", "Cumbernauld", "13219"),
    RawUser("Tiger Gross", "ante@necligula.co.uk", "1-901-938-7309", "7864 Elit Street", "Rimouski", "96348"),
    RawUser("Keaton Potts", "ac.turpis.egestas@semper.org", "1-102-599-3807", "P.O. Box 132, 1743 Nulla. St.", "Marburg", "36371"),
    RawUser("Jesse Osborne", "enim@Quisque.net", "1-980-466-7654", "5107 Pede, Road", "Petit-Hallet", "11832"),
    RawUser("Upton Irwin", "sed.pede.Cum@ipsumnon.com", "1-383-497-6678", "693-9067 Torquent Avenue", "Tournefeuille", "52668"),
    RawUser("Tobias Weaver", "velit.Aliquam.nisl@enimgravida.edu", "1-949-174-4248", "P.O. Box 674, 6969 Magna, Rd.", "Genk", "95278"),
    RawUser("Salvador Ortega", "sagittis.augue.eu@semperdui.co.uk", "1-272-113-6941", "Ap #133-7721 Sit Rd.", "Cheyenne", "73214"),
    RawUser("Noah Mcconnell", "eu.augue.porttitor@Pellentesque.ca", "1-162-835-8039", "Ap #486-6514 Mi, Ave", "Vedrin", "83705"),
    RawUser("Herrod Ayers", "Pellentesque.tincidunt.tempus@etnunc.co.uk", "1-620-731-7339", "4114 Libero Avenue", "Chimay", "10530"),
    RawUser("Matthew Thornton", "mattis.semper@Pellentesquetincidunt.net", "1-305-810-7982", "P.O. Box 184, 5281 Tempus Rd.", "Atlanta", "21983"),
    RawUser("Ira Molina", "morbi.tristique@orci.net", "1-446-359-0581", "229-1417 Dis Av.", "Masullas", "36453"),
    RawUser("Dennis Myers", "dictum.Proin@elit.net", "1-944-548-3631", "P.O. Box 842, 5423 Tellus. St.", "Freital", "75563"),
    RawUser("Wesley Berger", "tincidunt.pede@gravida.edu", "1-149-210-5545", "366-4454 Cursus Street", "Knighton", "42218"),
    RawUser("David Donaldson", "ipsum.Curabitur@tellusnon.org", "1-715-362-4668", "P.O. Box 326, 6499 Vel Avenue", "Pointe-du-Lac", "71736"),
    RawUser("Wesley Olsen", "et.arcu.imperdiet@tortor.ca", "1-104-808-8877", "877-3284 Sagittis Road", "Seydişehir", "78606"),
    RawUser("Jackson Mullins", "ridiculus.mus.Proin@porttitor.net", "1-900-239-8222", "Ap #195-6334 Elit, St.", "Kimberly", "45255"),
    RawUser("Cole Wyatt", "molestie.in@vestibulumneceuismod.com", "1-909-908-8040", "5579 Erat. Rd.", "Long Eaton", "78376"),
    RawUser("Amery Steele", "nisl.Nulla.eu@sodales.org", "1-123-712-7565", "Ap #208-9324 Quis, Ave", "Chieti", "47143"),
    RawUser("Alfonso Delaney", "vitae@egestasAliquam.com", "1-662-715-5307", "3189 Egestas. St.", "Yungay", "48890"),
    RawUser("Carl Ruiz", "vestibulum@luctus.co.uk", "1-541-281-8869", "P.O. Box 768, 7920 Molestie Road", "Beauwelz", "89054"),
    RawUser("Barry Crosby", "Sed.diam@Nullafacilisis.co.uk", "1-386-251-7964", "P.O. Box 535, 8058 Quis, Rd.", "Panketal", "40171"),
    RawUser("Aladdin Guy", "convallis@dictumaugue.net", "1-861-184-5889", "620-4646 Suspendisse St.", "Ambleside", "76885"),
    RawUser("Owen Simon", "Lorem@ullamcorperDuisat.com", "1-693-760-4948", "P.O. Box 978, 1686 Arcu. Road", "Shreveport", "62221"),
    RawUser("Bevis Fields", "vulputate.nisi@tempus.edu", "1-720-490-6886", "550-6656 Varius Av.", "Graz", "54835"),
    RawUser("Edward Meadows", "eu@laciniaat.org", "1-967-882-4533", "5336 Lacus. Rd.", "Belfast", "21345"),
    RawUser("Declan Moody", "adipiscing.lobortis@semmollis.co.uk", "1-106-781-1301", "879-4570 In, Rd.", "Rhayader", "21433"),
    RawUser("Armand Kramer", "Phasellus.ornare@egetodio.ca", "1-733-438-2633", "106-8521 Nulla Street", "Valtournenche", "77539"),
    RawUser("Todd Fisher", "lorem.tristique.aliquet@ornarefacilisiseget.org", "1-571-342-6987", "901-2662 Integer St.", "Pocatello", "63586"),
    RawUser("Dante Bird", "mi.Aliquam@magnis.net", "1-748-381-2078", "2286 Suspendisse Av.", "Terneuzen", "76094"),
    RawUser("Henry Ford", "quis.arcu.vel@egestas.org", "1-901-753-0825", "7375 Magna. Ave", "Aberdeen", "50176"),
    RawUser("Declan Blackburn", "sit.amet.consectetuer@lobortisauguescelerisque.net", "1-646-403-1665", "Ap #352-4988 Sollicitudin Rd.", "Poggio San Marcello", "90459"),
    RawUser("Oren Hale", "aliquam.arcu.Aliquam@vel.co.uk", "1-460-615-7976", "P.O. Box 364, 9288 Feugiat Rd.", "Weißenfels", "94634"),
    RawUser("Carlos Harris", "consectetuer.adipiscing.elit@ProinultricesDuis.com", "1-628-201-9673", "9629 Egestas Avenue", "Bad Neuenahr-Ahrweiler", "84173"),
    RawUser("Malachi Fry", "dignissim.pharetra@felis.edu", "1-777-900-9929", "Ap #487-8208 Sed Rd.", "Soria", "44359"),
    RawUser("Gavin Galloway", "Ut.tincidunt.vehicula@Namtempor.com", "1-853-941-9154", "Ap #537-4182 Est. Street", "Rotem", "23312"),
    RawUser("Hop Morse", "porttitor.tellus.non@augueid.co.uk", "1-748-885-0297", "7213 Lobortis Av.", "FerriŽres", "19955"),
    RawUser("Hayden Ryan", "dignissim@PraesentluctusCurabitur.org", "1-828-150-3220", "8191 Nulla Av.", "Rivière-du-Loup", "88655"),
    RawUser("Lewis Fry", "adipiscing@Phasellusfermentumconvallis.edu", "1-265-841-5383", "Ap #308-6249 Proin Avenue", "Boston", "73173"),
    RawUser("Julian Buckner", "mollis@mattisvelit.org", "1-539-140-1047", "Ap #420-6540 Tristique St.", "Lakeland County", "85910"),
    RawUser("Macon Haney", "nascetur.ridiculus.mus@mattisInteger.edu", "1-806-749-2795", "261-1885 Est. Avenue", "Horsham", "87820"),
    RawUser("Joel Foreman", "mi@et.com", "1-812-266-1175", "270-3286 Augue St.", "Buckingham", "70555"),
    RawUser("Zephania Tanner", "ipsum.Donec.sollicitudin@nostra.com", "1-536-368-8194", "7282 Egestas Rd.", "Pulle", "63373"),
    RawUser("Brandon Turner", "Sed@a.edu", "1-259-237-3821", "255-825 Donec Av.", "Vegreville", "89142"),
    RawUser("Driscoll Ewing", "vehicula.risus@ullamcorper.org", "1-147-963-7666", "P.O. Box 727, 4051 Ac Road", "Goderich", "73648"),
    RawUser("Simon Goodman", "ornare@erat.net", "1-354-988-7754", "P.O. Box 534, 6462 Pretium Road", "Armo", "26208"),
    RawUser("Hu Salas", "Nulla.eu@pretiumaliquet.org", "1-308-644-2896", "1018 Aenean Ave", "Gdynia", "71656"),
    RawUser("Abbot Harvey", "vitae.dolor@interdumfeugiat.com", "1-889-357-1589", "3353 Sit St.", "Rivire", "16935"),
    RawUser("Garrett Lawson", "Nulla.eget.metus@laoreet.edu", "1-337-513-2732", "6095 Eros Rd.", "Meix-le-Tige", "11157"),
    RawUser("Benjamin Levine", "semper@sempertellusid.net", "1-367-371-7771", "512-8900 Donec Road", "Campitello di Fassa", "47430"),
    RawUser("Damian Larson", "in@ornareFuscemollis.net", "1-498-843-7062", "P.O. Box 234, 2203 Ligula Ave", "Logroño", "41571"),
    RawUser("Ezekiel Prince", "ac.urna.Ut@metusAenean.org", "1-971-203-9102", "386-495 At, Road", "Lairg", "81468"),
    RawUser("Vance Buckner", "aliquam@placerataugue.edu", "1-349-600-6268", "P.O. Box 383, 7035 Porttitor Rd.", "Sargodha", "75905"),
    RawUser("Charles Ross", "facilisis@libero.co.uk", "1-410-219-4512", "Ap #702-3802 Est Rd.", "Clauzetto", "43154"),
    RawUser("Jesse Mcclure", "In.at.pede@tristiqueneque.ca", "1-863-278-8416", "601-3413 Lorem, St.", "Inverurie", "77262"),
    RawUser("Robert Garrett", "magna@consectetueradipiscingelit.co.uk", "1-205-513-2144", "9802 Orci, St.", "Mobile", "82448"),
    RawUser("Asher Sheppard", "sagittis.felis@et.net", "1-778-105-4304", "Ap #661-2405 Nec, Ave", "Snaaskerke", "16149"),
    RawUser("Rudyard Clarke", "sit@penatibus.edu", "1-145-653-2780", "575-8885 Libero. Avenue", "Columbia", "40583"),
    RawUser("Len Booth", "Curabitur@arcu.com", "1-258-131-2361", "P.O. Box 102, 2187 Neque Rd.", "Saint-Honor�", "44393"),
    RawUser("Stephen Donovan", "ipsum.primis.in@dui.co.uk", "1-432-906-1540", "2571 Velit. Road", "Derby", "62658"),
    RawUser("Lyle Crosby", "arcu@Nullamvelitdui.com", "1-367-836-8888", "9047 Odio Street", "Nurallao", "59367"),
    RawUser("Lucian Langley", "odio.Nam@massaVestibulumaccumsan.net", "1-410-284-4447", "P.O. Box 489, 1085 Libero Av.", "Henderson", "96821"),
    RawUser("Blaze Clay", "vulputate.lacus@volutpatNulladignissim.com", "1-726-294-0579", "858-4043 Vitae, Avenue", "Knoxville", "82277"),
    RawUser("Nathaniel Kirby", "Curae@egetmassa.com", "1-356-907-2894", "Ap #519-7722 Phasellus Road", "Lathuy", "91462"),
    RawUser("Nasim Bryan", "libero.nec@libero.net", "1-631-655-7295", "543-8590 Aliquam Avenue", "Kanpur Cantonment", "54711"),
    RawUser("Malik Scott", "placerat.augue.Sed@Quisqueaclibero.edu", "1-146-778-5936", "P.O. Box 661, 5677 Tincidunt Road", "100 Mile House", "50356"),
    RawUser("Jerome May", "molestie.tellus@tellus.edu", "1-269-921-5589", "7883 Parturient St.", "Girifalco", "77262"),
    RawUser("Channing White", "erat@semmagna.ca", "1-785-290-9592", "P.O. Box 161, 9738 At Rd.", "Posina", "11216"),
    RawUser("Burton Coffey", "arcu@ligulaelit.ca", "1-775-328-5408", "4496 Ac Road", "Sant'Onofrio", "58274"),
    RawUser("Russell Rose", "dapibus.quam.quis@purus.net", "1-648-302-0300", "4406 Est, Av.", "Liedekerke", "62749"),
    RawUser("Elliott Matthews", "metus.Aenean.sed@PhasellusnullaInteger.ca", "1-135-472-0268", "891-769 Suspendisse Road", "Castello di Godego", "96725"),
    RawUser("Amery Holden", "faucibus.lectus@sedturpisnec.net", "1-399-303-8748", "Ap #778-1201 Suspendisse Av.", "Coatbridge", "27973"),
    RawUser("Silas Hess", "ultrices.Vivamus.rhoncus@magnamalesuadavel.org", "1-687-821-9906", "1751 Adipiscing. Rd.", "Nanterre", "40201"),
    RawUser("Nehru Fitzgerald", "ante.dictum@hendreritconsectetuercursus.edu", "1-540-617-3646", "P.O. Box 492, 6328 Aliquam St.", "Temuka", "96921"),
    RawUser("Zachary Barr", "Nullam.lobortis@sociisnatoquepenatibus.com", "1-177-276-4590", "2842 Dictum. St.", "Sanluri", "21345"),
    RawUser("Damon Duffy", "Integer.in.magna@ornare.com", "1-990-929-4927", "P.O. Box 747, 5096 Nostra, Street", "Kawawachikamach", "66334"),
    RawUser("Zane Shepard", "Donec.nibh@Cum.net", "1-804-762-5946", "2577 Vestibulum Street", "Valpelline", "60429"),
    RawUser("Holmes Curry", "ultricies.sem.magna@imperdietnon.org", "1-336-633-9986", "539-5910 Dictum Ave", "Charny", "33521"),
    RawUser("Wing Potter", "consequat@Quisqueimperdiet.edu", "1-669-735-2219", "4252 Commodo Av.", "Malbaie", "35623"),
    RawUser("Keith Chavez", "Etiam.ligula@Curabitur.edu", "1-112-107-6174", "Ap #298-7976 Lectus St.", "Stene", "34636"),
    RawUser("Aladdin Estrada", "metus.urna@id.net", "1-413-459-2087", "Ap #576-8241 Id Road", "Camrose", "11723"),
    RawUser("Amos Carey", "sed.dui@Pellentesqueutipsum.co.uk", "1-285-437-0752", "784-4170 Neque Road", "Fort Smith", "39950"),
    RawUser("Ian Wood", "Sed.eu@estmaurisrhoncus.co.uk", "1-726-611-4659", "521-1785 Egestas Rd.", "Belcarra", "73779"),
    RawUser("Kadeem Jensen", "eros.Proin.ultrices@Donecsollicitudinadipiscing.org", "1-677-934-6135", "295-8084 At Avenue", "Indore", "43175"),
    RawUser("Jasper Powers", "Mauris.quis@Donec.org", "1-840-428-8519", "133-650 Cras Av.", "Ludwigsburg", "15305"),
    RawUser("Channing Welch", "Suspendisse.tristique.neque@etmagnis.com", "1-576-492-0869", "348-6059 Hendrerit St.", "Tiruchirapalli", "33843"),
    RawUser("Shad Everett", "erat.in@lacusCrasinterdum.com", "1-827-766-1966", "478 Velit Street", "Geest-GŽrompont-Petit-RosiŽre", "98937"),
    RawUser("Martin Kelly", "dolor.dolor.tempus@maurisanunc.edu", "1-568-234-4421", "960-6068 Eu, Street", "Los Vilos", "51181"),
    RawUser("Ivan Nguyen", "turpis@Suspendisseseddolor.net", "1-259-745-0261", "Ap #407-4780 Erat. St.", "Columbia", "85220"),
    RawUser("Joshua Oliver", "ut.pellentesque.eget@laoreetlectus.edu", "1-861-894-5360", "668-6348 Lobortis Ave", "Kingston", "60073"),
    RawUser("Beau Bond", "ipsum@Integer.net", "1-697-366-5554", "Ap #693-1876 Sed St.", "Te Awamutu", "34212"),
    RawUser("Oren Leach", "Mauris.non@consectetuerrhoncusNullam.com", "1-648-405-0040", "2977 Mi Ave", "Redwater", "22566"),
    RawUser("Wade Gross", "eget.tincidunt@seddictum.net", "1-628-791-9838", "Ap #935-3677 Fringilla Rd.", "Glossop", "87479"),
    RawUser("Sebastian Lindsey", "egestas.Duis@fermentum.net", "1-152-874-2732", "Ap #660-8944 Mauris. Rd.", "Lithgow", "82283"),
    RawUser("Daniel Crane", "semper@convallisligula.ca", "1-757-678-2972", "1395 Ac Rd.", "Castel Colonna", "90935")).toSeq
}
