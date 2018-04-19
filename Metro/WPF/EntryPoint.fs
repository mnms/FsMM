open System.Windows
open Program
[<EntryPoint;System.STAThread>]
let main _=Application().Run<|buildMainWindow()