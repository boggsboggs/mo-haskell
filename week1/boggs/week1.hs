import Data.Char

-- Lowercases all the characters in a string.
myToLower :: String -> String
myToLower []  	   = []
myToLower (x:xs)   = (Data.Char.toLower x) : myToLower xs
	  where toLower = Data.Char.toLower


-- Capitalizes first characters of words; lowercases the rest.
myCapFirstLetters :: String -> String
myCapFirstLetters [] = []
myCapFirstLetters fullString@(x:xs)
		  | isAlphaNum x = (myCapWord . fst . splitFirstWord $ fullString) 
		    	           ++ (myCapFirstLetters . snd . splitFirstWord $ fullString)
		  | otherwise  	 = x : (myCapFirstLetters xs)

-- From a string, splits off the starting contiguous chunk of alphanumerics.
-- Returns a list containing the alphanumeric chunk and the rest.
splitFirstWord :: String -> (String, String)
splitFirstWord [] 	 = ([], [])
splitFirstWord ls	 = splitter ls []
	       where splitter (x:xs) firstChunk
	       	     	| isAplhaNum x = splitter xs (x : firstChunk)
			| otherwise    = (reverse firstChunk, x:xs)
			where isAplhaNum = Data.Char.isAlphaNum

-- Takes a single word, capitalizes the first letter, and lowercases the rest.
myCapWord :: String -> String 
myCapWord (x:xs)   = (toUpper x) : myToLower xs
	  where toUpper = Data.Char.toUpper


-- Type model for an HTML Document
data HTMLDoc	  = HTMLDoc Head Body
data Head 	  = Head [HeadTag]
data Body	  = Body [BodyTag]

-- Some groupings of tags
data BodyTag = Bodyp PTag | Bodyh1 H1Tag
data HeadTag = Headt TitleTag

-- The actual tags
data TitleTag	  = Title String
data PTag	  = Paragraph String
data H1Tag	  = Header1 String

-- The Renderable typeclass
class Renderable r where
      render :: r -> String

-- instances for the actual tags
instance Renderable H1Tag where
      render (Header1 str)       = "<h1>" ++ str ++ "</h1>"
instance Renderable PTag where
      render (Paragraph str)     = "<p>" ++ str ++ "</p>"
instance Renderable TitleTag where
      render (Title str)         = "<title>" ++ str ++ "</title>"

-- instances for the groups
instance Renderable HeadTag where
      render (Headt title)       = render title
instance Renderable BodyTag where
      render (Bodyp par)         = render par
      render (Bodyh1 header)     = render header

-- instance for a list of Renderable's
instance (Renderable r) => Renderable [r] where
      render []     	         = ""
      render (t:ts)   	         = (render t) ++ "\n" ++ render ts

-- instances for the tags that contain a list of other tags
instance Renderable Head where
      render (Head ts)	         = "<head>\n" ++ render ts ++ "</head>"
instance Renderable Body where 
      render (Body ts)	         = "<body>\n" ++ render ts ++ "</body>"
instance Renderable HTMLDoc where
      render (HTMLDoc head body) = (render head) ++ "\n" ++ (render body)


-- A test document
buildTestHTMLDoc :: HTMLDoc
buildTestHTMLDoc = HTMLDoc ((Head [Headt (Title "This is da title")])) buildTestBody

buildTestBody :: Body
buildTestBody = Body [Bodyh1 (Header1 "Yeah, Header!"), Bodyp (Paragraph "This goes below the header."), Bodyp (Paragraph "Another Paragraph.")]