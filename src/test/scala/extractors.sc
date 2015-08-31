trait User {
  def name : String
}

class FreeUser(val name : String) extends  User
class PremiumUser(val name : String) extends  User
case class DefaultUser(name : String, firstName  : String, lastName : String) extends  User

object FreeUser {
  def unapply(user : FreeUser) : Option[String] = Some(user.name)
}
object PremiumUser {
  def unapply(user : PremiumUser) : Option[String] = Some(user.name)
}

object DefaultUser{
  def unapply(user : DefaultUser): Some[(String, String)] = Some(user.name, user.firstName)
}

FreeUser.unapply(new FreeUser("Damien"))

val bob : User = new PremiumUser("Bob")
val rene : User = new FreeUser("René")
val damien : User = new DefaultUser("my name", "Damien", "GOUYETTE")

bob match {
  case PremiumUser(name)=> "Welcome back, dear " + name
  case FreeUser(name)=> "Hello " + name
}



rene match {
  case PremiumUser(name)=> "Welcome back, dear " + name
  case FreeUser(name)=> "Yo " + name
}


damien match {
  case DefaultUser(name, first)=> s"$name is $first"
}