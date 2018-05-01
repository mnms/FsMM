//#nowarn"58"//when use ObsoleteAttribute on module
namespace global
[<AutoOpen>]
module Utilities=
    type ObsoleteAttribute=System.ObsoleteAttribute
    let notSupported()=raise<|System.NotSupportedException()
    module Random=
        //Another consideration: https://stackoverflow.com/questions/1399039/best-way-to-seed-random-in-singleton
        type Random=System.Random
        let private a=lazy Random()
        let private get()=a.Force()
        //let next()=get().Next()
        let lessThan exclusiveMax=
            get().Next exclusiveMax
        let pick seq= //TODO:try replace to return seq, prevent the function returns only one random
            assert(not<|Seq.isEmpty seq)
            lessThan<|Seq.length seq|>Seq.item<|seq
    //module Seq=
    //    open Random
    //    ///洗牌——打乱顺序
    //    let shuffle __=let a=Random()in Seq.sortBy(fun _->a.Next())__
module Geometry=
    type Quantity=float
    ///1 dimensional
    type Distance=Quantity
    type Length=Distance
    type Line=Length
    ///two dimensional
    module Plane=
        type Shape=Rectangle of Length*Length|Circle of Length
        module CoordinateSystem=
            type Coordinate=Distance*Distance
            type Point=Coordinate
            type Line=Point*Point
            type Polyline=Point list
            module RelativeDirection= /// refers https://en.wikipedia.org/wiki/Relative_direction
                let isOn45Degree((x1,y1),(x2,y2))=x1=x2||y1=y2||abs x1-x2=abs y1-y2
                type Direction=Up|Down|Left|Right|UpperLeft|UpperRight|LowerLeft|LowerRight
                let oppositeOf=function Up->Down|Down->Up|Left->Right|Right->Left|UpperLeft->LowerRight|UpperRight->LowerLeft|LowerLeft->UpperRight|LowerRight->UpperLeft
                let getDirection(x1,y1)(x2,y2)=
                    if y1=y2 then if x1=x2 then notSupported()elif x1<x2 then Left else Right
                        elif y1>y2 then if x1=x2 then Up elif x1<x2 then UpperLeft else UpperRight
                        elif x1=x2 then Down elif x1<x2 then LowerLeft else LowerRight
            module Movement=
                open RelativeDirection
                let forward distance=Up,distance
                assert(forward 5=(Up,5))
                let move((x,y),(direction,position))=
                    let distance=1
                    let implementByRecordingByList=
                        let move direction distance __=(direction,distance)::__
                        let right=move Direction.Right
                        let calc=Seq.sumBy snd
                        let move2= ///Incompleted
                            move direction distance>>right(int position)>>calc
                        let move2= ///Incompleted
                            move direction distance|>fun a b->a
                        let move3= ///Incompleted
                            let from __=List.singleton(Direction.Left,__)
                            let calc=Seq.sumBy snd
                            from>>move direction distance>>right(int position)>>calc
                        let move4= ///Incompleted
                            let from=List.empty
                            let calc a b=Seq.sumBy snd a|>(+)b
                            from|>move direction distance|>right(int position)|>calc
                        ()
                    let implementByTailCall=
                        let from a ()=a
                        let move direction distance=from>>fun a->a()|>fun(x,y)()->x+distance,y+distance
                        let right=move Direction.Right
                        ()
                    let positions=[0;1;-1;2;-2]
                    let fix=positions.[int position]
                    x+fix,y+fix
        module Grid=
            type Grid=int*int
            type Index=int
            type Position=Index*Index
            let create w h=(w,h)
            let toSeq(w,h)={5..10..w-1}|>Seq.collect(fun x->{5..10..h-1}|>Seq.map(fun y->x,y))
        module ScreenVisualDirection=
            let isOn45Degree((x1,y1),(x2,y2))=x1=x2||y1=y2||abs x1-x2=abs y1-y2
            type Direction=Up|Down|Left|Right|UpperLeft|UpperRight|LowerLeft|LowerRight
            let oppositeOf=function Up->Down|Down->Up|Left->Right|Right->Left|UpperLeft->LowerRight|UpperRight->LowerLeft|LowerLeft->UpperRight|LowerRight->UpperLeft
            let getDirection(x1,y1)(x2,y2)=
                if y1=y2 then if x1=x2 then notSupported()elif x1<x2 then Left else Right
                    elif y1>y2 then if x1=x2 then Up elif x1<x2 then UpperLeft else UpperRight
                    elif x1=x2 then Down elif x1<x2 then LowerLeft else LowerRight
module Metro=
    open Geometry.Plane
    open ScreenVisualDirection
    open Grid
    type Size=Grid
    type Station=Position
    module Entry=
        type Index=Center=0|Next=1|NextNext=2|Previous= -1|PreviousPrevious= -2
        type Position=Direction*Index
        let getStation,getNumber=fst,snd
        let getOpposite(direction,index)=oppositeOf direction,0-int index|>enum<Index>
    type Entry=Station*Entry.Position
    module ConnectionBetweenTwoStationEntries=
        let areSame(a,b)(c,d)=a=c&&b=d||a=d&&b=c
        module DirectConnectionBetweenTwoStationEntries=
            let canConnectWithoutRedirect __=
                let getStations(a,b)=Entry.getStation a,Entry.getStation b
                getStations __|>isOn45Degree
        module DirectConnectionBetweenTwoStations=
            //let mapOppositeEntriesOfTwoStations a b=getDirection a b|>fun direction->direction,oppositeOf direction
            let canConnectWithoutRedirect=isOn45Degree
            let getEntriesForDirectConnect direction=
                let threeEntriesOnCenter=[Entry.Index.Center;Entry.Index.Next;Entry.Index.Previous]|>Seq.map(fun index->direction,index)
                threeEntriesOnCenter
            let getDirectConnections a b=getEntriesForDirectConnect(getDirection a b)|>Seq.map(fun a->a,Entry.getOpposite a)|>Seq.map(fun(entryA,entryB)->(a,entryA),(b,entryB))
            let enumerate a b=if canConnectWithoutRedirect(a,b)then getDirectConnections a b else Seq.empty
        let availableConnections entry remainStations network=
            let getLineStations connections=(connections|>List.head|>fst)::(connections|>List.map snd)
            //let remainStations=getLineStations line|>Seq.except<|stations
            let availableConnections=remainStations|>Seq.collect(fun a->DirectConnectionBetweenTwoStations.enumerate entry a)
            let built=network|>Seq.concat
            let unbuilt=
                let isBuilt a=built|>Seq.exists(fun b->areSame a b)
                availableConnections|>Seq.filter(not<<isBuilt)
            unbuilt
    type Connection=Entry*Entry
    module Line=
        type Line=Connection list
        let passes line station=line|>Seq.map Entry.getStation|>Seq.contains station
        let start stop=[stop]
        let link exsistings connection=connection::exsistings
    module Station=
        let random=Grid.toSeq>>Random.pick
        let randomSeq n size=(fun _->random size)|>Seq.init n
        let create a=a
        let countConnections station lines=lines|>Seq.filter(fun line->Line.passes line station)|>Seq.length
        let getEntry lines station=station,(Up,Entry.Index.Center)
    type Network=Line.Line list
    module PlayerOperations=
        module FakePlayer=
            module Random=
                //let connection stop line network=
                let line startStation network remainStations=
                    let followingConnections=
                        let f(startStation,remainStations)=
                            if List.isEmpty remainStations then None else //TODO:处理没有直线可连的情况
                                let a=ConnectionBetweenTwoStationEntries.availableConnections startStation remainStations network|>Seq.tryHead
                                let f(a,b)=(a,b),(Entry.getStation b,List.except[Entry.getStation b]remainStations)
                                a|>Option.map f
                        List.unfold f (startStation,List.except[startStation]remainStations)
                    followingConnections|>List.fold Line.link [] //TODO:[Posibly Improve Performance]确认fold不会计算整个list
                let network stations=
                    let line network startStation=
                        let line=line startStation network stations|>List.truncate 3 //TODO:[Posibly Improve Performance]确认List.truncate不会计算整个list
                        line,line::network
                    //Seq.unfold line []
                    stations|>(*Seq.shuffle|>*)Seq.mapFold line []|>fst //TODO:对shuffle在Seq链中的次序做更多测试，测试把shuffle放在mapFold后面是否还能在mapFold过程中得到正确的state
    type Metro=Station list*Network
    type Game=Size*Metro
    open PlayerOperations.FakePlayer.Random
    let sample:Game=
        let grids=Grid.create 99 99
        let stations=grids|>Station.randomSeq 11|>Seq.toList
        let lines=network stations|>Seq.take 3|>Seq.toList
        //let lines=[Line.random stations 4;Line.random stations 3;Line.random stations 2]
        grids,(stations,lines)
module Presentation=
    //type[<Measure>]m and[<Measure>]dm and[<Measure>]cm
    //let cmPerM=1000<cm/m>
    module Line=
        let width=1
        module Direction=Geometry.Plane.ScreenVisualDirection
        let calculate connections=
            let calculatePointsForConnection(a,b)=
                //let current,previous=List.head exsistings,exsistings|>List.tail|>List.head
                let applyEntryPositionFix((x,y),(direction,position))=
                    let distance=width*2
                    let positions=[0;1;-1;2;-2]
                    let fix=positions.[int position]
                    x+fix,y+fix
                let current,previous=applyEntryPositionFix a,applyEntryPositionFix b
                //let applyCurrentStopNumberPositionFix,applyPreviousStopNumberPositionFixcurrent=applyStopNumberPositionFix current,applyStopNumberPositionFix previous
                let isOn45Degree(x1,y1)(x2,y2)=x1=x2||y1=y2||abs x1-x2=abs y1-y2
                if isOn45Degree current previous then[current;previous]
                else
                    let getTurnPoint(x1,y1)(x2,y2)=
                        let xDistance,yDistance=x1-x2,y1-y2
                        let getDistance a b distanceAfterTurn=
                            //let areSameSign a b=a=0&&b=0||a>0=(b>0)
                            //let distance=if areSameSign a b then a-b else a+b
                            let a=a-b in if a>=0 then a-abs distanceAfterTurn else  a+abs distanceAfterTurn
                        if abs xDistance>abs yDistance then x2+getDistance x1 x2 yDistance,y2
                            else x2,y2+getDistance y1 y2 xDistance
                    assert(getTurnPoint(10,10)(20,30)=(20,20))
                    assert(getTurnPoint(10,10)(30,20)=(20,20))
                    assert(getTurnPoint(20,30)(10,10)=(10,20))
                    assert(getTurnPoint(30,20)(10,10)=(20,10))
                    assert(getTurnPoint(30,40)(20,10)=(20,30))
                    assert(getTurnPoint(20,10)(30,40)=(30,20))
                    assert(getTurnPoint(10,40)(50,20)=(30,20))
                    [current;getTurnPoint current previous;previous]
            let p remain=if List.isEmpty remain then None else Some(calculatePointsForConnection<|List.head remain,List.tail remain)
            connections|>List.unfold p|>List.concat
    let sample=Metro.sample|>fun(grid,(stations,lines))->grid,(stations,lines|>List.map Line.calculate)
//TODO:list/seq