import Test.QuickCheck
import Test.QuickCheck.Test(isSuccess)
import qualified Test.QuickCheck.Property as P (Result, succeeded, failed, reason)
import System.Exit
import Lib

data TestCase = TestCase String String Int

testedModules = [
        (dynProgEd, "Standard DP Edit Distance"), 
        (dynProgEdSlow, "Intuitive DP Edit Distance"), 
        (lazyDynProgEd, " Lazy Evaluation Edit Distance")
    ]

tester :: TestCase -> ((String -> String -> Int), String) -> P.Result
tester (TestCase a b d) tested = 
    let f = fst tested
        description = snd tested
        result = f a b 
    in if result == d then P.succeeded
	else P.failed {P.reason = unlines ["Expected: " ++ (show d) ++ ", instead actual: " ++ (show result) ++ " for" ++ description]}

main :: IO ()
main = do
	done <- mapM (quickCheckWithResult stdArgs{maxSuccess=100})	(tester <$> [
		  TestCase "" "" 0,
          TestCase "aaaaaa" "aaaa" 2,
          TestCase "cgggtatccaa" "ccctaggtccca" 6,
          TestCase "aaaaaaaaaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaaaaaa" "aaaaaaaaaaaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaaaa" 4,
          TestCase "abcde" "abde" 1
          TestCase "SJMYYSHZFVIJZNJKLORHKICWVBLPFZJZSBVPPDIBXQVCYQKTABQYQYAXSRKDIDDREYSOWOTZSOPQMSDVXITECQFVBXXXAHTIXYGMIESPEEHELWKRWQNGLDWXVATHJGPHDJLIHGHLFMDRVFCZDBWDTXNTQVWBZWLHSYVBUAFPXMLKHJYKCKYKIGSVVRNNFMHGQPQFLXSATYLFIOGAUZFNMQZAMIGLPOHXWTNYBBKDCQYEVUBWVEFGUWSYTSXRNWRQPWVGDTLYYEBIFWCINPNNXBLGFDQCUXQRDRTDMOFTMQQTGZQNNMTBKBTZXSEMAGXSFRLSMPDRACAUWQKMGFXYIWJUKMCSUCQPVQOSMCADAGNDMSXQDISKIMVCPWUEUCARWBTGTDRPPCPGXNXZIWGXQHOPEUMYGJWMLRLNGVJTDAVELWNOZRARVVQUKQCVFXVFWZPKSCBMUXOXZMSYJJWVLDZQFULXVDGSRGYQOWBVBIQUAKNVTKIJIBSZEIJMBOELYVFGELILNQLNCVXYGQDOJBSNFHCKHSPNQUAIMHRYVERFTSRGQCBKRGLVKFKFVZATPUHWWBFHZABMYJLCKWXZCXPMXFGBQKQSOEREHTHVNSFMRENGYHFZTJPCWGCESTMZEMJHMSGODYQUCDGXUIZYLSWGOAEUAGOOZOGWWCKZAMWZVXXFQRCAYTFLPNVAQVEEEXDXCIJZOSHWLUDBDKRZZQSJUJITYZCPAXERYMDCPORIYTOSGSLRWXECOWFTKJXWMDTECVKNLEYNITVFSULNFPSSELBYTJHSCDNKKLUWBZQFJRPKVSURMQNPUAQRSLZSODYXCKEPIYTTOCBFOQAHCRWSJOJLNIKSLRRMQDNIQDDREOWTBRBHHXQIVRHFPTBCAHPPXYNIVPJGUKYCASSDISQRJQJXAFOUIIOHXTMUYQIPDPXSEIQVSSVNGZHBPAUVAQSXFGJFIOJNNAXVVKKQZQOZMHXPAOFLTIUFQCXV" "SJCTMZZDYSPWZFZINJXXNRKXORHKICWOODBLPFZJZSBVPMDIBXQVUCRKTABQIYQYPAXKSRUWKDDQIDRDZBEOJSOFDWTSOQUSDVMPXTECQVBXNXGJAHTIYGIXESPHEALWWTYKRQNYDWXVKTHJGCLQPHDJLQHGHLFMDRVFCZDBYWTXNTBBWBZKLHTYSYBLPUAFPXMLKZGGJYDKCZEKCSKIGSVVRCNNFMRQPQFSILXQSUSATYLOPFOGAUZFNMQZAMIGLPOHDWWVNBKOCLDCQYFVJBWLTVCEQUWSPIYENSXRNICYWJHPHLYWVGGVDGTLYYKWPEYTHBIZFWIDZANPNNLGFQCUZIKQRDTRTUCDDMOFTMQQGZQJOQXFDNNMTBKBTCJEMAGXSFRLSUUNDRACAUQKJMGCIFXNDYIWJUKMCSUQVQZJGOSMADAGNDMSXQISKIMVCPWUEUOCWMARWODNBTRPTZCGXTXCIWAXTXWQOPQHOPEEMCWMLRLNVJTDALJVEZWNYZRARVVQUKUCVFMKXVFMPKSCBMUXOXNMYFLJJWVWINLNDZZSIIULXVIGRQYQWZSBRQUAKVKKJIOSOEWJMBEVFEILHNQLNCVGQDJBSHYKHSVNQUVAIVZKRYVEFTSRGQPCLRGLVKFVZAVATTXWPUHWBFHABMYJCKWCZCZPXPMXFGCOIBQKOROQSORLHTHVNAFRENGIHYVYFZTPCWGCESMPEZTJJJMODRQUCDGXUNZYLSWGOGESUAGYOZOGYWCKZCMWZBGSXXRMABUYNALNAQXOAVZDMUEEXDXCIJOSWLZDXNODNMRZZOJUJTYZCPAXEZZRYMDCCPORIVOSFBWSLRWZECYOOPTKSWIDZAJTDLEVNLEYNITVFSULNFEWSELBYTQHBLSCDNCMMKKLUWBZFJRKVOPSURJQNVASLSOLADEYXWHEIYTTOBFOQQAHMRWSJCJLNIKSLRORTQDNQDDREEABJRBHHXQFIVHURHFPTNCAHPPYZSPNKVJGUFKKLCASSDDJQISQJQJAFOUIIHXZETMHYSIFPDPJTAXSFXTEGQHSKVGGZLAHBPAUVALSXRWUFGAXBWFITOJNNAXVVKKQQOZFNTHPPCOFLTIUQCX" 431
		] <*> testedModules) 
	if all isSuccess done
	   then exitSuccess
	   else exitFailure