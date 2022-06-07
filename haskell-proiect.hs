import System.IO
import System.Console.ANSI
import Data.Char(toUpper)
import Data.List(sort)

---Crearea fisierului---
createFile::String->IO()
createFile file = do
    {
        tempfile <- openFile (file ++".txt") ReadWriteMode;
        hClose tempfile;
    }

---Adaugare in fisier---
addText::String->IO()
addText file = do
    {
        putStr "Introduceti textul: ";
        text <- getLine;
        appendFile (file ++ ".txt") (text++"\n");
    }

---Afisare text din fisier---
displayText::String->IO()
displayText file = do
    {
        text <- readFile(file ++ ".txt");
        putStrLn text;
    }

---Eliminare semne de exclamare, puncte, virgule---
remove::Eq a => [a]->[a]->[a]
remove = filter.flip notElem

---Functie din txt in lista de cuvinte---
sortWords::String->IO[String]
sortWords file = do
    {
        word <- readFile(file ++ ".txt");
        return $ words word;
    }

---Eliminare semne---
removeMark::[String]->IO[String]
removeMark wordList = do
    {
        return $ map (remove ".,?!_\"") wordList;
    }

---Convertirea la UP a cuvintelor dintr-o lista---
toUP::[String]->[String]
toUP []=[]
toUP (word:wordList)=((map (\car->toUpper car) word):(toUP wordList))

---Sortarea unei liste de cuvinte---
sortlist::[String]->[String]
sortlist wordList =sort (toUP wordList)

---Contorizarea aparitiilor cuvantului din capul unei liste sortate---
conapcCL::[String]->Int
conapcCL []=0
conapcCL wordList=(capCCL (head wordList) wordList)

capCCL::String->[String]->Int
capCCL _ []=0
capCCL searchedWord (word:rl) |(searchedWord==word)=1+(capCCL searchedWord rl)
                     |otherwise=0

---Stergerea aparitiilor cuvantului din capul unei liste sortate---
delapcCL::[String]->[String]
delapcCL []=[]
delapcCL wordList=(delCCL (head wordList) wordList)

delCCL::String->[String]->[String]
delCCL _ []=[]
delCCL searchedWord (word:rl) |(searchedWord==word)=(delCCL searchedWord rl)
                     |otherwise=(word:rl)

---Generare statistica aparitii cuvinte intr-o lista de cuvinte sortata---
statapc::[String]->[(String,Int)]
statapc []=[]
statapc wordList=(((head wordList),conapcCL wordList):(statapc (delapcCL wordList)))

---Contorizare efectiva a aparitiilor unui cuvant specificat
cntef::String->[String]->Int
cntef searchedWord (cc:rl) |(searchedWord==cc)=1+(cntef searchedWord rl)
                |otherwise=0


-- Lansare contorizare aparitii pentru primul cuvant dintr-o lista
cnapPCL::[String]->Int
cnapPCL []=0
cnapPCL wordList=cntef (head wordList) wordList

---Meniul aplicatiei---
menu::String->IO Int
menu file = do
    {
        clearScreen;
        setTitle("Analiza statistica a cuvintelor pastrate intr-un fisier text.");
        setCursorPosition 9 40;
        putStr "Fisierul curent: ";
        putStr(file ++ ".txt");
        setCursorPosition 10 40;
        putStr "1) Schimbare fisier";
        setCursorPosition 11 40;
        putStr "2) Adaugare text in fiser";
        setCursorPosition 12 40;
        putStr "3) Afisare lista de cuvinte din fisier";
        setCursorPosition 13 40;
        putStr "4) Sortarea cuvintelor in ordine alfabetica";
        setCursorPosition 14 40;
        putStr "5) Afisare frecventei cuvintelor din lista";
        setCursorPosition 15 40;
        putStr "6) Afisare text din fisier";
        setCursorPosition 16 40;
        putStr "7) Inchidere program";
        setCursorPosition 18 40;
        putStr "Optiune: ";
        keyb<-getLine;
        option<-return(read keyb::Int);
        if((option == 1) || (option == 2) || (option == 3) || (option == 4) || (option == 5) || (option == 6) || (option == 7))
            then return option
            else do
                {
                    setCursorPosition 20 40;
                    putStr "EROARE: Optiune eronata.";
                    getLine;
                    menu file
                }
    }    

---Main---
main :: IO()
main = do
    {
        setCursorPosition 10 40;
        clearScreen;
        putStr "Numele fisierului pe care doriti sa il analizati: ";
        file<-getLine;
        run file;
    }

------
run::String->IO()
run file = do
    {
        option<-menu file;
        if (option == 1) then opt1 file;
            else
                return();
        if (option == 2) then opt2 file;
            else
                return();
        if (option == 3) then opt3 file;
            else
                return();
        if (option == 4) then opt4 file;
            else
                return();
        if (option == 5) then opt5 file;
            else
                return();
        if (option == 6) then opt6 file;
            else
                return();
        if (option == 7) then opt7 file;
            else
                return();
    }

---Crearea functiilor pentru optiuni---
---Create file---
opt1::String->IO()
opt1 file= do
    {
        setCursorPosition 20 40;
        putStr "Introduceti numele fisierului dorit: ";
        file<-getLine;
        setCursorPosition 21 40;
        putStrLn "Se creeaza fisierul";
        createFile file;
        setCursorPosition 22 40;
        putStrLn "Gata!";
        getLine;
        run file;
    }
---Add text---   
opt2::String->IO()
opt2 file = do
    {
        setCursorPosition 20 40;
        addText file;
        getLine;
        run file;
    }
---Display word list---
opt3::String->IO()
opt3 file = do
    {
        words <- sortWords file;
        wordList <- removeMark words;
        putStrLn $ show wordList;
        getLine;
        run file;
    }
---Sort word list with UpperCase---
opt4::String->IO()
opt4 file = do 
    {
        words <- sortWords file;
        wordList <- removeMark words;
        putStrLn $ show (sortlist wordList);
        getLine;
        run file;
    }
---Display word frequency without marks---
opt5::String->IO()
opt5 file = do
    {
        words <- sortWords file;
        wordList <- removeMark words;
        putStr (unlines $ map show (statapc (sortlist wordList)));
        getLine;
        run file;
    }
---Display text in file---
opt6::String->IO()
opt6 file = do
    {
        setCursorPosition 20 40;
        displayText file;
        getLine;
        run file;
    }
---Exit application---
opt7::String->IO()
opt7 file = do
    {
        setCursorPosition 20 40;
        putStrLn "Ati iesit din aplicatie!";
        return();
    }