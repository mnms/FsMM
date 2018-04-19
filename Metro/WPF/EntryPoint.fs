//open Administration
//[<EntryPoint>]
//let main argv =
//    testDownloadAll()
//    //TaskQueue.Test.test()
////    English.OxfordDictionariesOnline.ExtractedData.Parser.extractAndSaveToFile originSourceDir
////    English.OxfordDictionariesOnline.Mp3Download.fixFileNames()
////    buildDataPack();testWriteWords()

////    let a=findIndexOfMatchedPageTitles csvSourceFile|>toList
////    testCheck()
////    buChong()
//    printfn "%A" argv
//    0
open System.Windows
open Program
[<EntryPoint;System.STAThread>]
let main _=Application().Run<|buildMainWindow()