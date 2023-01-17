01->13: subtype(4010) 	 1
                Tuple{L, Tuple{Int64}, Int64} where L<:Union{Int64, Tuple{Int64}}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R<:Tuple{Int64}, LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                
                
---------------------------------------
02->13: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R<:Tuple{Int64}, LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
03->13: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: R<:Tuple{Int64}, [2(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: R=Tuple{Int64}, [2(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
04->13: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: LT<:Int64, [2(0,0,0)]: R<:Tuple{Int64}, [3(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT=Int64, [2(0,1,0)]: R=Tuple{Int64}, [3(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
05->13: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where LT1<:R1
                [1(0,0,0)]: R1<:Tuple{Int64}, [2(0,0,0)]: LT<:Int64, [3(0,0,0)]: R<:Tuple{Int64}, [4(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: R1<:Tuple{Int64}, [2(0,1,0)]: LT=Int64, [3(0,1,0)]: R=Tuple{Int64}, [4(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
06->13: subtype(4015) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
07->10: subtype(4008) 	 1
                L				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
08->10: var_lt(6001) 	 1
                L				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
09->10: subtype(4009) 	 1
                Union{Int64, Tuple{Int64}}				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
10->10: var_gt(7006) 	 1
                LT				Int64
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
11->12: subtype(4009) 	 1
                Tuple{Int64}				Union{R, R1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
12->12: var_gt(7006) 	 1
                R				Tuple{Int64}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
13->13: subtype(4012) 	 1
                Int64				Int64
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT=Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
14->24: subtype(4010) 	 0
                Tuple{L, Tuple{Int64}, Int64} where L<:Union{Int64, Tuple{Int64}}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R<:Tuple{Int64}, LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                
                
---------------------------------------
15->24: subtype(4011) 	 0
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R<:Tuple{Int64}, LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
16->24: subtype(4011) 	 0
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: R<:Tuple{Int64}, [2(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: R<:Tuple{Int64}, [2(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
17->24: subtype(4011) 	 0
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: LT<:Int64, [2(0,0,0)]: R<:Tuple{Int64}, [3(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT<:Int64, [2(0,0,0)]: R<:Tuple{Int64}, [3(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
18->24: subtype(4011) 	 0
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where LT1<:R1
                [1(0,0,0)]: R1<:Tuple{Int64}, [2(0,0,0)]: LT<:Int64, [3(0,0,0)]: R<:Tuple{Int64}, [4(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: R1<:Tuple{Int64}, [2(0,1,0)]: LT<:Int64, [3(0,0,0)]: R<:Tuple{Int64}, [4(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
19->24: subtype(4015) 	 0
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
20->24: subtype(4008) 	 0
                L				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
21->24: var_lt(6001) 	 0
                L				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
22->24: subtype(4009) 	 0
                Union{Int64, Tuple{Int64}}				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
23->24: var_gt(7004) 	 0
                LT				Tuple{Int64}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
24->24: subtype(4014) 	 0
                Tuple{Int64}				Int64
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,1,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
25->39: subtype(4010) 	 1
                Tuple{L, Tuple{Int64}, Int64} where L<:Union{Int64, Tuple{Int64}}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R<:Tuple{Int64}, LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                
                
---------------------------------------
26->39: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R<:Tuple{Int64}, LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
27->39: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: R<:Tuple{Int64}, [2(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: R=Tuple{Int64}, [2(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
28->39: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R1<:Tuple{Int64}, LT1<:R1}
                [1(0,0,0)]: LT<:Int64, [2(0,0,0)]: R<:Tuple{Int64}, [3(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: LT<:Int64, [2(0,1,0)]: R=Tuple{Int64}, [3(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
29->39: subtype(4011) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where LT1<:R1
                [1(0,0,0)]: R1<:Tuple{Int64}, [2(0,0,0)]: LT<:Int64, [3(0,0,0)]: R<:Tuple{Int64}, [4(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,0,0)]: R1=Tuple{Int64}, [2(0,0,0)]: LT<:Int64, [3(0,1,0)]: R=Tuple{Int64}, [4(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
30->39: subtype(4015) 	 1
                Tuple{L, Tuple{Int64}, Int64}				Tuple{Union{LT, LT1}, Union{R, R1}, Int64}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
31->36: subtype(4008) 	 1
                L				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
32->36: var_lt(6001) 	 1
                L				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,0,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
33->36: subtype(4009) 	 1
                Union{Int64, Tuple{Int64}}				Union{LT, LT1}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
34->36: var_gt(7006) 	 1
                LT1				Tuple{Int64}
                [1(0,0,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
35->36: subtype(4009) 	 1
                Tuple{Int64}				R1
                [1(0,1,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<:R1, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
36->36: var_gt(7006) 	 1
                R1				Tuple{Int64}
                [1(0,1,0)]: LT1<:R1, [2(0,0,0)]: R1<:Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<:R1, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
37->38: subtype(4009) 	 1
                Tuple{Int64}				Union{R, R1}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
38->38: var_gt(7006) 	 1
                R				Tuple{Int64}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,0,0)]: R<:Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
---------------------------------------
39->39: subtype(4012) 	 1
                Int64				Int64
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}
                [1(0,1,0)]: LT1<Tuple{Int64},R1>, [2(0,0,0)]: R1=Tuple{Int64}, [3(0,0,0)]: LT<:Int64, [4(0,1,0)]: R=Tuple{Int64}, [5(0,1,0)]: L<:Union{Int64, Tuple{Int64}}