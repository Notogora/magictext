{-  findAll searchWord readfileString rowList wordList
    PURPOSE:
    PRE: True ?
    POST:
	VARIANT: Length of readfileString
    EXAMPLES:
 -}
findAll :: [Char] -> [String] -> [String] -> [String] -> [String]

findAll word [] _ wList 	| wList == [] = ["No match found"]
							| wList /= [] = reverse (wList)

findAll word [string] rList wList 	| word == string = reverse (("On Row "++(show ((length rList)+1))):wList)
									| word /= string && wList /= [] = reverse (wList)
									| word /= string && wList == [] = ["No match found"]

findAll word (string:xs) rList wList
		| word == string = findAll word xs rList (("On Row "++(show ((length rList)+1))):wList)
    | word /= string && string == "\n" = findAll word xs ("\n":rList) wList
    | word /= string && string /= "\n" = findAll word xs rList wList



 {- findFirst searchWord readfileString rowList
    PURPOSE:
    PRE: True ?
    POST:
    VARIANT: Length of readfileString
    EXAMPLES:
 -}
findFirst :: [Char] -> [String] -> [String] -> [Char]

findFirst word [] _ = "No match found"

findFirst word [string] rList 	| word == string = "On Row "++(show ((length rList)+1)
								                | word /= string = "No match found"

findFirst word (string:xs) rList 	| word == string = "On Row "++(show ((length rList)+1))
									| word /= string && string == "\n" = findFirst word xs ("\n":rList)
									| word /= string && string /= "\n" = findFirst word xs rList



{-  findX searchWord number readfileString rowList wordList
    PURPOSE:
    PRE: Number may not be negative
    POST:
	VARIANT: Length of readfileString
    EXAMPLES:
 -}
findX :: [Char] -> Integer -> [String] -> [String] -> [String] -> [String]

findX word _ [] _ wList 	| wList == [] = ["No match found"]
							| wList /= [] = wList

findX word x [string] rList wList
    | word == string && x > 0 = (reverse ("On Row "++(show ((length rList)+1))):wList)
    | word == string && x == 0 = reverse (wList)
    | word /= string && wList /= [] = reverse (wList)
    | word /= string && wList == [] = ["No match found"]

findX word x (string:xs) rList wList
    | word == string && x > 0 = findX word (x-1) xs rList (("On Row "++(show ((length rList)+1))):wList)
    | word == string && x == 0 = reverse (wList)
    | word /= string && x == 0 = reverse (wList)
    | word /= string && string == "\n" = findX word x xs ("\n":rList) wList
    | word /= string && string /= "\n" = findX word x xs rList wList
