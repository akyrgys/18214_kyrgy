import Data.Map as Map
data Person     = Auth String String MusicBand | NonAuth String String | Musician String String deriving (Show, Ord, Eq)
data MusicBand  =  ClassicGroup {basist, gitarist, solist, barabanshcik :: Person}  | OrchestraMan {foreverAlone :: Person} deriving (Show, Ord, Eq)

linkinpark  = ClassicGroup (Musician "Chester" "Bennington") (Musician "Rob" "Bourdon") (Musician "Brad" "Delson") (Musician "Mike" "Shinoda")
eminem  = OrchestraMan (Musician "Eminem" "Eminem")

bands = [linkinpark, eminem]

type Song = String
songs :: [Song]
songs = ["Numb", "Rap God"]

bestSongs = fromList (bands `zip` songs)

getlname :: Person -> String
getlname (Musician _ lname) = lname

getFavoriteBand (Auth _ _ band) = band
getFavoriteBand (NonAuth _ _) = error "you are not authorized"

getNextSong (Auth _ _ band) = Map.lookup band bestSongs
getNextSong (NonAuth _ _) = error "you are not authorized"

getBestBand (Auth _ _ (ClassicGroup basist gitarist solist barabanshcik)) = Prelude.map getlname [basist, gitarist, solist, barabanshcik]
getBestBand (Auth _ _ (OrchestraMan foreverAlone)) = [getlname foreverAlone]
getBestBand (NonAuth _ _) = error "you are not authorized"


me    = Auth "Artysh" "Kyrgys" linkinpark
notme = NonAuth "X-man" "Y-man"  