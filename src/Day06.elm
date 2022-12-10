module Day06 exposing (part1, part2)

{-| Day 6: <https://adventofcode.com/2022/day/6>

Given a string buffer, look for a marker represented by a certain number of unique characters in a row.

Part 1 wants 4 in a row, Part 2 wants 14.

Report the number of characters received by the time you get the marker.

For example:

    bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5 (part 1) or 19 (part 2)
    nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6 (part 1) or 23 (part 2)
    nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10 (part 1) or 29 (part 2)
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11 (part 1) or 26 (part 2)

-}

import List.Extra


samples : String
samples =
    """
mjqjpqmgbljsphdztnvjfqwrcgsmlb
bvwbjplbgvbhsrlpgdmjqwftvncz
nppdvjthqldpwncqszvftbrmjlhg
nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
"""


type Part
    = Part1
    | Part2


type alias Buffer =
    { chars : List Char
    , location : Int
    }


solve : Part -> List Int
solve part =
    input
        |> String.trim
        |> String.lines
        |> List.map String.toList
        |> List.map (findMarker part { chars = [], location = 0 })


part1 =
    solve Part1


part2 =
    solve Part2


findMarker : Part -> Buffer -> List Char -> Int
findMarker part buffer chars =
    let
        markerLength =
            case part of
                Part1 ->
                    4

                Part2 ->
                    14
    in
    case chars of
        [] ->
            0

        c :: rest ->
            if List.member c buffer.chars then
                findMarker
                    part
                    { chars =
                        buffer.chars
                            |> List.Extra.splitWhen ((==) c)
                            |> Maybe.withDefault ( [], [ c ] )
                            |> Tuple.second
                            |> List.drop 1
                            |> push c
                    , location = buffer.location + 1
                    }
                    rest

            else if List.length buffer.chars == (markerLength - 1) then
                buffer.location + 1

            else
                findMarker
                    part
                    { chars = buffer.chars ++ [ c ]
                    , location = buffer.location + 1
                    }
                    rest


push : a -> List a -> List a
push a list =
    list ++ [ a ]


input : String
input =
    """
mvwvrwvwbblffmvmhmllmzmqzzbhbnnvrnvnmvvqrqrccpsstvvshvvpvvpddzwwjddfzzgbbzcchrrtzzmfffzqqsjjbfbjfjwffhrhrgrfrsfrsffbwwphwhgwgmgnggzwzhwzzpggnccdbcbssgccqgcgcvgccwffrqfqtqmqppwphhlbblnnwhwfhwhmhzhmmzbmbbjjmcjcggwlgltldttpccjlccbjccvfcvvdbvdvndvvmbmcbmmhphpbhhgjgllmljlqlwlclzlccrvrdvdzvzzzbrbrnbblplrplpsstdtjddgjgjgwjwzwbzwwrhwwbzwbwpwssvgvnnwnvvjqqswwsccwctwtntdndffjzfzmfzmfzmfzzhjzhzssdsgsnggbbqdqlqbllmglgddqppjjtzttlflrrczcmzzsppvfpvffcfrccvvvhjvhvfhfbbwdwrwswqqftfbfcfdfnfpnfflbbcmcqqsqwqqccmwmvwvffvdvlvtvqttnllqslsnllzplldmmzczhzmzzhllpfpqpnnqwwwrzzdpzzhccfvcvclcnnrjrmjrjlrlflqqtpqqdgqghhwdwdttlztttwmmvfmftfptphhddswdsdpdvpvpcpjjnvjvqvhvqqqfgqqgwqgwghwwmmvmvfmvvnvsslcclzlslppztzfzbzllcgllcsllvgvqvtqtsstztwwgjgghnhnhdnnqgnntqqtccdmcmhmpphpmmmbpbptpnnnmnppwlpwwfpwfwmfwfssbspslsflljpppbsspwptpwttfbtfftltvlttdmttllmggfcgfccvwcvczvvgvsgswsrrwppgcpggpgnngfnndrrdjrdrfdfmmjmsmbmmwjjjfzfdfbdbbtgbbcsbccgbbrvvrqrwqwmmhrhhbcbdbvdvzdvdjvvnwntwwdfwwrhrmhhnghhncccfttmpmrrjhhqddrsrvvssmrrgvgdgmmfqqhttspswpwlplpjljcjmjmvmdvvhbvvtqvqccczwwtlllztlzzpnnqznzddqsqvsshrrmccnppgrrjcjpccrvcvnvcncllffvnnbsbhhrmhmzzzhrhchjccgjcjwwhddmfmhhmrrrbccqddmjdmjdjtdjjmttzstsctssvvgghgbbhmhzmmjqqqqjgjfjpffpmpzmzszfzgzcggpmpnnqjqffgpghhszzvlzzdgzggqmggqlllcwwtzwwcfwccpjpggtqgtttzffdpffjzfzddzppprnprpqqbppvjpjzppbzpbzppwzzcnndsdzszbssqwqggnhggnzggpcpnndssghsggmfmrffvddvwvssjmjrmrmnndwdwhdhwwfjfjwjmwwqssmdsmdmssqzzrgzzjttnqnfqfcccvvmsvsvffccdjdccgtgqqjwqqnbqqnhqhrqrmrgmrrzjzrznzllwrlrbbcppctptwtfttlntnltnlnzzhrzhrzrtzzvsvfvjffsbsnbbfpphtptjjmffqgqnggnqnsshffslflggtlggmpggnfgfbgbfbcbbtdbdjqwfggzsfmzflttgdfqchhtjfwlmdnsbvmqcrhsrtwtjlmnlwbvjvqdzswdthbjslqzgmtzfjfcrgbhrrjtzgljbqfrzzqszhddnmzpnbgnflghnflwdmjdhgpnvwvnltcngwggqdznrpdtwsrclwwlfzhnjtpqcgzjqjnhcwbhndwvgrczhzwfjrdvjztdbjshmhrrqctjwpcdnpnvrtggrmhzhdtlntjphcddplgfvvmjzcbbpjbqbjwmnwqgftbmchghwvrlptvnfbffvgtdbtnfzwdmdlgzjbrvffvbrfwgjzzpbpcdzhcjbhfwnmqqwvjvnpgdqdjsmwdmrgfqrwhhqqjpfmvlncfdjchrccwpbptccdchqwvbcqzlhbfgrdzgncbwrzqpfszrcwtmnvbztjlzjlrqqfrnplhnmdjljcpnqssthhmjqrrlwdvsjswdwtfstmbvwhbptjnphjmnllbwffppzmdpchbcwcmgqzfmdqvhcmrvtgtjwlzfpnjnjtfvdhtjlqsdjwrrnflnsqrtfsnbjfdllhqslltsjqzdfqthqjjgrtqjwmlqqlhvszqswmdcbnwqgshpvfjdtvsqjvcnrnvtpfsgqszhtmcdlsqjwmttcqsdlvssrgwhtmtqwmttptnmgzpwzzrbwtsrjhmjpblztgftppcwwrjppvwlvdhwwdlfcdvhwpvhqpwrqsczvlmtghrrvqbphljcmcrfmwltlmnzchzrlbgspdjwfbhrmnfhvjwlgtghcpbfgdvrmwbqbprfvwpzqzrgqbhjtztwhjcjtncmbgjphsgdfvjwjljlwzhsdldqtdvgtzpwtmrbnpvqrmfdwngzqtsdjjztslrwdnfwgbnsjblcjzvmgbqllmdvvvdcvplgbzmhcrpbbjbfzhjgfpmjtrpmgvshzvqhcbcjzfcsvqggjcllcnjlrwqfpzldswzjgzvqwvszhrntnvlcpdcfqhqrtmhbdjqpfblrsbrhdfdwfgbhsnnjpgjvfpssfmhdbdncfrqbzpbttrhfzqnrttltqbvmhglbdpwmdbmgwcdsdsflmcnphwvbbhgqpqmwbdsmqwhcdftdlcfnstlfnzzsjhqzlsdmhpvlcqvhhlpdtzlqzlvbbwhhdsqhntbtpjjvnjlrncfmvnqmzwpldgrbfcfgdtlmrfzcbqfhvhpmsrrlnqwfggwrjsmnpnqhvvcfsfspfrmhwmbpqfhprzsswrczttlczhcqvgqqsnbhhfszqbswgtcjgddhngtcvlwqqmqzntrcwgwjhmhlgclpgtmqnpgwwhnjfdvvjgmqjlzsmwztpsdzrjlshswzljlmdzfqmqbtgtzlsfwqvwrdchjvrdnwdbprfvdvczstvnzfzzfmzbwjhtflbmhlgmfzrdfrqwfbltwqlqrghlprppvlqwggvczdcnmrmblhcfmpzdmqppgwhbbrjlzsvmsfzlrdljftcrdfdgmvrccwszpjmvdnbmfdnsgmgzsdpbrlsqvsmcwwqlwtwbgfvwtsmpqlnhjchnzrpncgtcqgwqrmqmrwsbmdvqcpggpzcjgtrhvnbpprlwfnjlvrgrdjjvncjmmflrpczqnlbqczggssqfcjcrghqlsztpdjpbbfcdjzzdjbljlsczdmrgshdscvhnltrwchjcmjzmjbhldnqzwwpswsnsldcbdcdpdgpjgwrnfbcpjtvzlhvgggldcfqcwwppvltsvsmwzwdgmhnfggtgtwldmrglcvmstgmbgbjhwwhgdhfrbjlwfhjfppdmffblbbzrplhlhqlsrnsthvjtglqntzcncmvhqnlsvvrscrhlncgncjswfgcvgjlwsndzmsbhbdqsggdgrsfqtzwnjpsdlbsqjrrjwwlbptwqpfqwvpsrtrmstddzdbjjlwvfqcpfczpsnmcgbfpbwcpljdhrgsqftbnplccjphwsdbprqfqqfcvtcdznnhrbdqpccphvdgtspmzzdbnslnrvtfrtbhcfzfgmhrttmdpwftvccjbllhqgtmpgwvbdjgtvtfbfnnsfgzqjmrpbcmqhpfbstznbvgtbhnwbbnjfsthdgdrpfdtvrtmgbwzqqpnlltbshjvnhsmqhqwzgbsfqqccfznjtnnzdrgcvwnlffdgvvqzvwhwfswdmqlrsglntzsnwjzgrhhwzzshwsfmlmrbnmrqlptmjgmtqctrmddzghsgtrbbcsmhtcnrzwmvrjmrnmhbjmflrclvlbzwbgmtnmwqgfmbbnnrdvhqcflglvzbmjzjtvnmrbgghnccfrphjshsgtrhfmmghhpwgclfvzfbdccsfrlfwtsjjnhlndpwcjdtlllhcsvwrwsbppqwhfcvnsnrvthrsbgmgjhpmjdndmdqdgzfvbqfmgfjrnrjchstrjprfwfnjqblhjdgsstvtpcsvmpbhggnwzncpjdhrcllcghhprhwhfgsqpfzphrdlcbrccglsb
"""
