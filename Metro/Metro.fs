//#nowarn"58"//when use ObsoleteAttribute on module
namespace global
[<AutoOpen>]
module Utilities=
    type ObsoleteAttribute=System.ObsoleteAttribute
    let notSupported()=raise<|System.NotSupportedException()
    module Random=
        //Another consideration: https://stackoverflow.com/questions/1399039/best-way-to-seed-random-in-singleton
        type Random=System.Random
        let lessThan exclusiveMax=
            let a=lazy Random()
            let get()=a.Force()
            //let next()=get().Next()
            get().Next exclusiveMax
        let pick seq=
            assert(not<|Seq.isEmpty seq)
            lessThan<|Seq.length seq|>Seq.item<|seq
    module Seq=
        open Random
        let shuffle __=let a=Random()in Seq.sortBy(fun _->a.Next())__
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
        module Grid=
            type Grid=int*int
            type Index=int
            type Position=Index*Index
            let create w h=(w,h)
            let toSeq(w,h)={5..10..w-1}|>Seq.collect(fun x->{5..10..h-1}|>Seq.map(fun y->x,y))
        module RelativeDirection= /// refers https://en.wikipedia.org/wiki/Relative_direction
            let isOn45Degree((x1,y1),(x2,y2))=x1=x2||y1=y2||abs x1-x2=abs y1-y2
            type Direction=Up|Down|Left|Right|UpperLeft|UpperRight|LowerLeft|LowerRight
            let oppositeOf=function Up->Down|Down->Up|Left->Right|Right->Left|UpperLeft->LowerRight|UpperRight->LowerLeft|LowerLeft->UpperRight|LowerRight->UpperLeft
            let getDirection(x1,y1)(x2,y2)=
                if y1=y2 then if x1=x2 then notSupported()elif x1<x2 then Left else Right
                    elif y1>y2 then if x1=x2 then Up elif x1<x2 then UpperLeft else UpperRight
                        elif x1=x2 then Down elif x1<x2 then LowerLeft else LowerRight
module Metro=
    open Geometry.Plane
    open RelativeDirection
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
            let getEntriesForDirectConnection direction station=
                let threeEntriesOnCenter=[0;1;-1]|>Seq.map(fun index->direction,enum<Entry.Index>index)
                threeEntriesOnCenter
            let getDirectConnections a b=a|>getEntriesForDirectConnection(getDirection a b)|>Seq.map(fun a->a,Entry.getOpposite a)|>Seq.map(fun(entryA,entryB)->(a,entryA),(b,entryB))
            let enumerate a b=if canConnectWithoutRedirect(a,b)then getDirectConnections a b else Seq.empty
        //let tryConnect a b existingNetwork=()
        let availableConnections entry remainStations network=
            let getLineStations connections=(connections|>List.head|>fst)::(connections|>List.map snd)
            //let remainStations=getLineStations line|>Seq.except<|stations
            let availableConnections=remainStations|>Seq.collect(fun a->DirectConnectionBetweenTwoStations.enumerate entry a)
            let built=network|>Seq.concat
            let unbuilt=
                let isBuilt __=built|>Seq.exists(fun a->areSame a __)
                availableConnections|>Seq.filter isBuilt
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
                let line startStation existingLines stations=
                    //let startStation=Random.pick stations
                    let remainStations=List.except[startStation]stations
                    let a,b=ConnectionBetweenTwoStationEntries.availableConnections startStation remainStations existingLines|>Seq.head
                    let remainStations=List.except[Entry.getStation b]remainStations
                    let followingConnections=
                        let f(previousEntry,remainStations)=
                            if List.isEmpty remainStations then None else
                                let station=Entry.getStation previousEntry
                                let a,b=ConnectionBetweenTwoStationEntries.availableConnections station remainStations existingLines|>Seq.head
                                let remainStations=List.except[Entry.getStation b]stations
                                Some((a,b),(b,remainStations))
                        List.unfold f (b,remainStations)
                    followingConnections|>List.fold Line.link [a,b]
                let network stations=
                    let line network startStation=
                        let line=line startStation network stations|>List.take 3 //TODO:有的有有的没有，需要更新line函数
                        line,line::network
                    //Seq.unfold line []
                    stations|>Seq.shuffle|>Seq.mapFold line []|>fst //TODO:对shuffle在Seq链中的次序做更多测试，测试把shuffle放在mapFold后面是否还能在mapFold过程中得到正确的state
    type Metro=Station list*Network
    type Game=Size*Metro
    open PlayerOperations.FakePlayer.Random
    let sample:Game=
        let grids=Grid.create 99 99
        let stations=grids|>Station.randomSeq 15|>Seq.toList
        let lines=network stations|>Seq.take 3|>Seq.toList
        //let lines=[Line.random stations 4;Line.random stations 3;Line.random stations 2]
        grids,(stations,lines)
module Presentation=
    //type[<Measure>]m and[<Measure>]dm and[<Measure>]cm
    //let cmPerM=1000<cm/m>
    module Line=
        let calculate connections=
            let calculatePointsForConnection(a,b)=
                //let current,previous=List.head exsistings,exsistings|>List.tail|>List.head
                let applyStopNumberPositionFix((x,y),(direction,position))=
                    let positions=[0;1;-1;2;-2]
                    let fix=positions.[int position]
                    x+fix,y+fix
                let current,previous=applyStopNumberPositionFix a,applyStopNumberPositionFix b
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