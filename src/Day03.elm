module Day03 exposing (part1, part2)

{-| <https://adventofcode.com/2022/day/3>
-}

import List.Extra as LE
import Set


part1 =
    input
        |> String.trim
        |> String.lines
        |> List.map String.toList
        |> List.map bisect
        |> List.map intersect
        |> List.concat
        |> List.filterMap toPriority
        |> List.sum


part2 =
    input
        |> String.trim
        |> String.lines
        |> List.map String.toList
        |> List.map Set.fromList
        |> LE.groupsOf 3
        |> List.map
            (\sets ->
                case sets of
                    [ a, b, c ] ->
                        Set.intersect a (Set.intersect b c)

                    _ ->
                        Set.empty
            )
        |> List.concatMap Set.toList
        |> List.filterMap toPriority
        |> List.sum


{-| Split a list in half.

example: [1, 2, 3, 4] returns ([1, 2], [3, 4])

-}
bisect : List a -> ( List a, List a )
bisect list =
    let
        half =
            List.length list // 2
    in
    ( List.take half list
    , List.drop half list
    )


{-| Get the intersection of a tuple pair of char lists.

Example: (['a', 'b', 'c'], ['d', 'c', 'b']) returns ['b', 'c']

-}
intersect : ( List Char, List Char ) -> List Char
intersect ( list1, list2 ) =
    let
        set1 =
            Set.fromList list1

        set2 =
            Set.fromList list2
    in
    Set.intersect set1 set2
        |> Set.toList


{-| Return the priority value of a char, which is based on alphabetical order.
-}
toPriority : Char -> Maybe Int
toPriority char =
    let
        alphabet =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    in
    char
        |> indexIn alphabet
        |> Maybe.map ((+) 1)


{-| Return the index of the first occurance of char in string.

Examples:
indexIn "acab" 'b' -- returns: Just 3
indexIn "acab" 'z' -- returns: Nothing

-}
indexIn : String -> Char -> Maybe Int
indexIn string char =
    string
        |> String.indexes (String.fromChar char)
        |> List.head



-- INPUT


sample : String
sample =
    """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""


input : String
input =
    """
gfWpjRRQffQGCHHJsGqjsj
SclzJZZvmmnPbJtVSqqNBqVCBdSCsd
tlbvZJDZtmtPcJmlPnhMFQWWpMRFTfLDRRTWRp
HjMPgSWjVrjgbHRRSSMRgjRdpdbGdlcdCvQfcCdlwQJfdf
LNDnhtNtLNFFZDtFnhzvdldDflvvDCdlJfldpJ
ZFLFZZmFtFtTNTSPRrVPWWMpRP
qLBSBLRwmgzqCbzCffDlrfCV
TFFFHNWFMFFMpHpGHMTHGNhrldWZCsdZsslZlZfrflDVss
PTMcPGntTThHhTGctnMvSwjjvmmqLBmnjqqgCR
nClJtMwwntqVVPJcgZqq
mjpsDcrcSSFFPZqFBWWgVP
vQcjsvhrvvrmhbmNHMNnlHbNMtCtNM
bgvvhnTQtjrrrhsDDf
pLSMltLzLLSjFrSSjrSJHD
zNWRLBdZPllPQtCvttgCqb
DRlDrrFTNDNlgzsGTBfcnqhhcnJfcrCSqc
MMmmdWtdLmvtldHjMmQfPBqSJWnfCCCqcWSSPJ
vjHMjLmjpLtHptQLmHvwTRgNVVpTzZFZgZRlsVTN
rzpMpDCGFCFFjRFsRPFRNFPv
fWclbHCHtSmfvjnmfsvZ
wTcTlSwwtQtWclBQBLGMLMCLVzVLwJGqLd
MQSjLNjPPLLSBPjfQhSPHjDVCjDtVVpDHwbwVpbD
RcmWzsRrzZrmTszWRqWlmRJscbtHwCbndCtcDVddDpdnVnbt
JTsrGGTqmwTlWmTzJzWmhhPLLGgPFgBffSSPhFFM
qMMRNZMDDNWLPqfzCgDcGncVDCgG
wwBFhwhhBgmcVzhghG
tbJbjjtJvwtdtwjpFtlbvtdTLNSMqNqMMgqNHPlZRTNggL
qmjMHsZmZSbjbZMjSLFFFFwgsgvFswpwww
hRJBhmnhhvFFwhcv
llfWDWzrzBNTRfNBrWzzTmZbGTMjPqMmZPjVbSZGSP
CRRPLwwcclcGVppQ
SHFjDjjHDTfSDNTTHfSHjQVGrpmllQQWltVVVZGp
HFlqzDTfqlzwbgPJLwCP
WRCNLphpLppSCWVHNfLRzVnQMnBnMddPMQDFQgrhPQFM
jTjJqvqjvPVJFJFBJF
qTsZbvGqqZlstsmZVljtwqwSHHNWczHSSRcWNSRHzzNfbW
glgzDzHjSrVHcVgbrjmNsscNGmNWssGNNtst
hHPQLHJpwdLpdHfQQtnZmNMwnZGZWwsFZM
QpdhPJRTJfPphJfhCBlVqVvgvVDBbvVqDbHD
VtHzjZpjVtHrprgGmjHsGHNdSJFQRcLJqCdQcSqJNpcq
bBWfTPwhbfDlMnhffRwQJQNdqJcLFQLSdR
bhBhvfMWTnlDnTBfPSmvmjsjmmGtzHtsHm
pcRPRPWrSDcJGZSStmwZZS
VnLfCfTlfVzfnMMBCqVNZJdtjNtJjhJdGNNbwT
BLvqCCMVsnRQsPQgDcZH
cQbqqQhDGhlQfQlhQrqGsTNgLgCpRgLTPPPLNbpg
wtHVddVFwSHznZwwznCpRBdjppNBNTTdCjRR
ZtWFwWtSmvVnwZDrCMGfQlDDJQmD
PzPZGCZzrZrlhdjdCqfCsqQdRD
cbvZLVVFvbbNSNFHSDnsDQdnfqNQDRngsR
FJHSLSFSScJJbWHFmFVFSZmrrzBmhtBwmzBMPMPzPh
nlpFcLBgcVcLbssGVBGGrlpGPhJJJJJqPBZPDNMQMJJhJQZZ
SSTjHzfHwtZSPVQVQMRQ
TzVHwWfTtzwdVzsbFnGgsbdcGrLc
FppVBRVZDdLmrDGmmfrQ
NtNMPNshJCzznLGJSrqRrRrr
tRssthhPlCWhPzsWtzhzCbVVjwTpVwdZZTpwjbdBbwBc
TTWblHWScvPCCHTWFzSrqqsNNSmdmqrrpz
RLRwjjnjZNprzmmZcq
QQgtQnccQDGjgLDRRcLthQhFBvCbMtMHTWlBFllBbFCMTW
WnBVNvDnVsNvZWdrWDLVDMbsHpTjpHCSSClsbSCCMH
GPFtmztzgPhRFtJTdbTwjppSCjpgSl
hJcfPtQhdtWNVZqNnqNQ
GLcqZPPsnqQcFsmBBrqRvrddNqrC
MtHthJwLllwvjRvvtrvBRS
VHMfDLbpfznszZQG
WBSdPlQPRfBtGQPfBGPBJgzgjwsJzsszJwCrdwCT
ZpppVpMVpnVHMVVbZRJrCgwRzTJrwNJw
MvhmnpLqLmhVmBlftRQBFSlR
hhQlSJqhtCSnqZJnqShSlNDwRzpvdwRlMBMMdcjRjMpMRc
frrGmLmWbfFrsmFHmBzBvBcwdJbvpjzbMM
mmgFrVGLWJLFGsgfhSVtVPqntqnnSStN
SFJTJTSqswwFQbwf
cDtcWPclrtPwVsfssQmN
HDtwWCgWdggdzSGJMSzGMq
JpqJtWRJMhCMJpMQCWtFrjgHdgdlgllwNjlQjldH
fBzPZcZvnBmDnZvZBZDmPvglVVVdgHHSwrNRgVgwNPRH
GbZnZccfvcsZmccsmnnZTRbCCMWFTWJqFCCMJFRT
vrrFqrFTBTmLmNrLMqMTHddJbHpWnhdWdWbHhJGM
wBzfwzcQSzWSSshpdWGp
gwjPPPDQtzQlzQDPqTgLBRmRqZBvqFNR
bWVptFFsbPcZsGLhsZGmLB
qnWrnrHdMCDCNqfWmvRRZSSRLdRGZGRG
nNqqNDfMrMWHDQNHzWfHNDnwzblpzFlbwtFbVVlwVcPJpP
BHJhlHdJQggvddglJBBhglhQzZHPZpFFPDMzFDDRDFZZDFZD
rSTfqnCffMfCVfCLNqbzbjWNDbbWDPFpPFbP
nfnnrSfCTVSwrqSLCGfTGlgQhlvsGMJQJBhhssJhGc
tBjjDjjqfDjLfJlrLgglvmrlmrcc
TwNNTVhwwpgvGSNNSssS
TbwhnvvChhbVRTPPRJBJQQfJttMQQJCQfW
mWSvSQVgmWQsQvspQJlrlLnJLLpCClhhlp
bFHRjZdNjjBZzFzhtnCllCcJLrCBll
HFFNHbdZZLZjfPFjHVQmWDDVsvsmTqVqDf
JJPllQQClqgBCgdHwHbpjVTwHd
tmGZtjGjHZpVbfMT
ShGjNGWmDSNcNRtGmshDRzzCvzQJJRBLrvlrBPJv
cTpqsTWqVVpsNLfvCDFlMFDVFL
JnndJPddQgzHlvMJFDhLCG
BjtntgdRnQgzjdBRQBlpNWrTTlNTSwNpWS
qHmqLVLjmVqsDBLtmjmbtPwCTwwPzGWRgGwGwMwW
ZhcCNCSprRTWTwSnWW
hflhZvvQhppZfcNpvrhpQHjVjLmbVmmVHVCFDvqVFb
nnNrwDnZrspwDNnZsNSDsNbCmpjvMTPQjLMmPmmQPGBTQP
FdVtRdRfctBQPmTtTLQB
qhzWVWJqVHwbhlLSsS
htWmhDhFztnztDhtBmBtghPRSrpfjVwPdfPwpwnRSVrr
cbCHvgJGcTqbqcbqqqcqsMsRVrSCwffdRPPpVpwCRSwfjj
GlgGQqTqbgQzttmBNNFz
NWQNQgdTgjQNddTZfrCQWRDnnnbqnLqnRcjJlqqvDj
FtSSmSmJhpllcclDvpln
JBVVSsSFBVBttShFGSPQfCGNdrMfZZTQTZNNdC
HgHthMhphcbfbMMfHhsGGDCRRVlcVSScsCRz
nWvPFqLqPNdjnNLnjdJnPdWjGlssDPSsllVCRzlTCTGlSDzS
RvddJRJQHwQwpZZb
gdZwgpjZZQtHTdrWrwdpWRnlhNBRlLbFthNhflhBnL
CVzDCPGMVqVmGsGGbJCmCDvMcRcqnBFFFnRBBNRBBNqhnFfF
DsmSGsGPzvMGJvdbgTSTbjbSSdgH
jBGmbNBQGdBNNDJNQRLLVDsHtDRzHHZZcH
wCWPFWPCrPhPrplvprhwpCHHtszttqZslRVHLtzVlJZL
vprMMvMnJCwnnPShNGSTfGSfNmmgdNff
bPtLbvVWWztbLSVVnbszpzQsrcDDBdpRcDrs
llZmgCZqgCFgmdRdJcscBdJsmQ
FZlgfqCFfgZHlqCMCglwCFGWntLLSMRSPGPVttWRtVGL
vtnDsDtrnrSvrMVmbrrJgPCmBm
FpQHzFclLVzWHhwHLQLlHLzPmMBQCJTdTmCTmBTJTTmgQg
pllcVWqlffZqZtZD
TSSZWpsQmZWcTZSvsTTTppNPzrBPrNBrzQNVFrBBNPqP
CgjmCbtGgftMmLtLmffzBzJJJNVVMNzNBqJrFN
gjgjLgtLwgbGjHdhhGdvmlnllnpWnplZvcvwTl
htLrRFRtbbhlGSLRtbJBJsjBmgMMgJgtmBzz
pZQWddQQfpZZffcDQZwddQwDMqDDsPgGJJzzjqzgJMBJgmms
QdcQTdwpGNwfrCRlRVlNLSbb
wrdvpVBVpMGPPjWjGZJJZT
tChCSlNfCCHtvHHWPHPZ
RbRRNvmcqcblfMwwdVBQQqqdpL
qcctqRcqmcHWzHBdDMZhfwthBnwt
JFsSNMSgNSNJJMGJBBdjhFDfhwhBrwnZ
TbgbsSgJMTJllblLCSPlsTCVQmRVVWpQzzqpqzVzHLQzcc
CVcWbjjSSCSSnpjWpCpprhHZlHtHGzHrZrHGclrl
gqZqdddLgmgNqvTGGHvvmrrGHT
FFDgZfZNLMgNfdDqDRnsnjBpbSbnMBBWpQpB
qwpQFwRnqFFfSBSfFt
LJJLGLWWtZlbgWHgGshhSdSVzmhHmfVzzC
lrbrbrNNJgDMLLbblGctvvvDqPcqctTTTcqP
vnblvbfHvlcHMlHlZbSPLTPLwCMBRRPRRFFR
tszzBqtzDsWVPRSmzLVmVL
tsNsDDNgGsqBrgBpgdHQbfhflcHdpZvdbh
cCpLtpGGLsgsppcpmGGHMtjfHRVhvvVVFRfhjV
NWnnnNNndQnQZdCdzzRVMHzvhhHWWWjj
CPJJrnSZpGDJLGTL
cnJzpcnmnQVFbzTlvTHBlb
tWCDPjfsDGfZhddhjjdTvFTgFgvbnFHvdHqT
hjfCjwDDGjPthsfhsnGNrJcQcRmJMLVJrJNMLw
CPPRrSlRccPcwTHwfdwTHdfl
mLQLLjhQhhQLZvpzssHDhdTswzzTJD
gmjbBvQLWmgbQZBCSRnnnSMVCBHnBS
sWrBJbsVqschzhQzHh
gtFmztnSlSfdlmnZSdSwcwGRTjcTcwwTcHccRg
FzFDzMZCdDZtCSrJVBMqWVrqNBqN
TvWlhhfhZJVgtSSl
ddBdGGdFmmBbdzqqPDDGGmdDZSgttHtZppSgzZHSgMhtMgtz
PGqdrbbbdPnrcjjhTRWLLc
trrmJWcrVwVbcPScdcBdGPHH
JTQnfjlJTpQFfMLlNJHHGDPdGsSdDjHGDPPH
ffFfnCTTCfTlplTMvNVzqWvwVzrrhwmWhJbW
hVtDtgcghzJpmmhlwp
srsnrqqsPqsBPvnqRBRMPbnwlplpmCStJwmzJPtJzJfwSw
bbrqjBbvGsjGGBWqMVFFVDNVNjZjgtgFgZ
mnmhBDHhwWCHsTgRsH
dcSlFvccMFMMFFggNsTzzvvzWnVW
llQdllZScFplJPpdcZSqBqjhmtnrwrDGnQGhrq
ZffVNgfTdmPVltsnnGwgQDnB
rMCFLMHpzCMFzHpzbrcHFLzBwsDsDDnlDBJrDDBBSJSnBn
MLMjMzqpCzvwqTmwZdvq
DDNlWPRqgPRPsRFjJQZbchJZbgQJ
zzrLLznpLbHnjcBHvVvHvJcZ
ndmrTzbMMTfzrTfnTLrzdpmsPPPqlqGDNNsPCRDRqRsD
zzdqTNfTfdfhgQhgqMFSjRDtDRWHqtWlwtqDRS
ssBCrcmpVGZvVRDdSDRwtmWdDb
rvGPCZLCVCPVBZFdnfThgNgLJNhf
bslcrssQwDPbQrrcsbsnQrjMLthPMMRhLRhLRgzmgPhRgM
DffvDfHGfNFdpfTdMtghLBThzVmBhBtM
SNvJNJdflDDbcDWJ
HFlHNpWsTlGWbFsGFTGHFLLNzPPhLVPMzVzMNPhhzP
jSvZtmrqqpcrCpPVzw
dddQvqDgDmjdSQQdqZjStpffWGgBRWTGfGsRlWBlHF
THnTbNrdBnLTHHnTnBrWRTndsccZsLZcDqmLDPcDlQDsmmsZ
ptwzzhpvGSVdqQlmszqmqPqc
wGVjSddCBggCHFWN
LFFbdbhhhvwvfTNdRhhRRvMbHDGjcfcGfDjtDHHcHqGjDqqj
WlQnVpWSSWWsPsgDqDzHDLHjJcttGP
rrWsZrgVnWrWSlmSlmSBFFbvTThhBFvvZLBhRw
BgBdcjThvjFcTggrqvVfzlnnPlrqLt
JpwJGPsQwpwSssHpPLlzlnNlzLLNNLVtsN
JPMmWGmWPmHbHpJbWGJmDmwbBTRZMBBdZCRTRjFjhCZCCBTT
BjbcLFRfBRhnbGjCVVvPllpcPtcDmdlPpvPP
WrMQqCNgsqWWsTNCMZMWWsWPvJDJDddvlpDtZDpDDDDwvP
qNMzzSzSQsGLbFCSCnVR
tTRpHJQpQBZcddhhMhvhJN
zswljflgMFbwPqmNmSdvShLNfLhm
qFbsMCVgsqMwRWHCWDDBDWpt
VSTCCWsJvGpHHCNC
GrqzZrrZjDljcDDlfjMqgRPfPvQPpBHNvHvBpvNQ
rljncDcznjMqhlhZDnltrzhTsGWtbVLFTTWGsbdWJdFTmL
mJPDSJJPZPJNrprSNrDmpZGrhFFhBqjGbGGVbFjhhfqBjBRV
cgnTQHdMQdTHdhqfggBhVqVfVS
nQdLLddssSJrmsNvZrPz
jfjffQzZQQMzZZfZZQFgjDWBCRlCBdTTBGGGRpBCgdhdBG
LrstWtNsbHLsprRBdlGpCwlh
HLnntbnscqLvvPNNfMWSSmDMDPjzjDzS
vhcGwWVvglltcfBn
BBSLrzSJLzJNJrLfPfPRsmDRmflD
jMjFZJNMqzrzZzFNFjNQqJzbCpBBvWdpvTCWhpVwdvHVCGbG
HlrnFmRmtRBQPVBTQHHQ
psSLJsLpTTdPdLTv
fCGgTgfSSCtRtFFzql
pfTpStppcDlWfbpDdzQRsQGJhfffQgJHzN
ZFZFZmBFwVwBVmLmLsRLRhHNzRLRNNzJ
FnnjwVPmnqqqjBjrTdblldCTpcPJtbTD
bdZHdWlrjslMMwGG
rDDTRBTqSqmJLBJRBTSJpmMsMMjhwvfMhjjfVGsLshhC
BqQFRPFRQBJgzrcZNHFdZt
wrDdLlDdPWZPTTrwlZpSsPsHVHsSCHnbzMHM
JtNFttNCjFvpppnMpJgSVS
NFFqFcCQCvfrZmGdZdmqrW
GMNNfJnNddJFJWsv
HSDwCmmghLmwmmHDpsvdFpMWpppptSbp
zCzBCgzhwmhzLrPnVrMqZBNfGf
DrHGtbltbCjjjffPrgsmzmcqsgDczdsmgJ
VZLwQLZLLVwLBQZnLVphhLQQqsTNmzJdcNTzzmJNqlNBsszz
wZLhVMplpQVRRlpVGPfjCjMGCrbHGWWb
BHpFrHHbBNTWWTWNhCPwPLNPjCdjLV
zJRRzJvZlcZsSMJdzSDjDtfDCtDtjDjjjj
dcJcszQJJGRJzRllMpGHpFTWmrTmBTbWWB
qnWWqhDhnjmjCMBlNRrfVfRNCB
vvBLBtGHJTHBddrNVJrVSVdr
BZLTHbgvHvTFBgTFFvhmWmmZDPmmZDsnqncs
WBvmjDbSzTMmHHdpNHNF
ttlflZRfGtfWVRltGtflCdHnJrNJHNHnJddNMNCnpF
VVwssWQQfRGZcszBQzDbjSBvSBDP
lSlQqQVqWWVWfqQWVJSTscdmPPwwTTmjjfpjPp
FCbzHbvHvtgrtFCvbvbbwdTwmsrwnTTpmdswmwcc
DtZbHdghztlLMQlWWhVQ
pqzzFSmdFqbQvlpdDGGrGBWPPBVNQnVttZ
cgcjwfBMhHCjjLMCrtcnPcsnsPGVnrVs
JgCChjjjBHhRRLLjjhplzvzpSFJvzzlDbSqm
mZzVQZMhmrffwfQhWhzmrmpBtRcdbnbcdcMpBbDbncdD
jsLTSlTWRBSDpnDn
GLTsGWGFsfmJGZVJZm
BGWshBGnsFWSLWBLlSSLWRJHnrVPrPcNHCNHctnPPJ
QmvQCqqMTZqvgmvTjpZCMgMtrVctPptHtrNVrptbJJbrRP
CzjCZfCwDzShDWdF
HmQlQHmJnpmptmzt
MTqMjMPvTvVvhpdztZnSwzwZqS
CcbLLPTMtCCsjHNHQFLRRFlRNN
GDFwLLLLSrbdPlFBMFsslFHmZH
TnJCgthHpVTfZMQZQmzWnZ
hjvtjtghtqJvVjhTgNhJTvdvdDDRbbccrwPdcGwrHS
MQQMBPzMGQBPBbDQPMhpnRwsGnRhNrFFpRnF
vmgHcmCTTlvvvZvTmqcTfmCRdddFnwdRdnVwFpVfpRnwNw
gvmqJTcHclCQJNzjMLWbLj
DbqqDDbQFqfNtZSLSq
RrdjPdmrpWBdmWRdccfLtNttSDMZBfftLMLf
dCcgmgRrWcgcppjCVVVVFHFnDnbJnb
fZMFfrtVdZSDVwTgjRMLhwTCLj
cNzPBNpclllzHbmTNRhqCRTgjC
nhhWJzhGPlQcGvsvfJtSfZfrtt
PSzrBWQBBGzBlnSnWtDrqHfNfwVwHcLNjHjwcDNmFH
hbRhtRCRpRvsRgVVVcNHNNNCwLwc
ZtRTRvttWWzBPlGZ
tcLnctNsJrWWNDTN
pwPPSjHSHHfzvmSvvvFVVGqGVqGmFqrDWgDr
pPSvfPQMzCQCSbhllLnQDhbtQZ
DmLffDhpVhjjVwvbwNVFbbNSNH
JRPBgMPRHBrMHMHqrBMqWJBSQQNbCvndNrdvCNCFwFrQnv
WcqJcPGMGtWRRBtgZjjspGHTLHGHTppm
ptJtWJpqRwDZZDVWpbDWqlvvflfMjlfCMjdCCdtslv
rLwTBGBzBBQTzmwCCjvdvlLllddsMl
NBwTmrGNgrTrcgPpWgWPDSVVPW
CdglMnrlSSqDPpcsZb
ccwmVJtvVvVtNhBpBFPDVpqbbD
TRGQjJjGTmtrTCgHWLfrcn
JNNhLwWwWQHNPDmmjHpc
zMqZCvVCSMVqMSTVvZVGsBnlslpmsmzlPmsHPsPB
qTVqrgdCCbhfHJQFtg
wNwCBBCZsfQWfmLCGSmmFRGSSF
zjnPHPVqMhhZLTcbpbSncp
lVlhlgzlPZlwtgBddJdfvf
JWRWRRLWJLnjtjnLzGzznflBvfPvPMqMDqdbzblCzC
TTScTVbHmTsVFrmcsgcHFlPMMvlvrDPdlrDDqdldvl
bVpcpchgsFZHbhSmSTsHFFjwtZjnjLttntNjLjNLWtjw
rffjPJzWzrgPpGWHVNqTtmqFTVRH
cswhvlLBvSLsCtbFccmqVFNTbb
wwZSCZSnCLsSDGgDmpGnfmmr
rTfJTNtjfNljlrWSlzRtNlTqsddwGnsnHHwwhssTsnqw
VpbpZZbvPLbZbbBhwqMHhsGMnJdVwV
mgQZJDLBJbbbcbgZClCSfWlrCjRjlDCR
fSpwcVfzsztcSSWNNMbnMRqTvtTv
mJFmGDDDhGhBJHCQddllqTvCllqTRRWNnMbT
FdFDGdDDDhhHdZDjhDmpwSPVZszpwZsVgsPRZs
"""
