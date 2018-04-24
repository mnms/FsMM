namespace global
module Program=
    open System.Windows.Controls
    open System.Windows
    module Draw=
        open System.Windows.Shapes
        open System.Windows.Media
        let draw(size,metro)=
            let rate=2*5
            let scale=(*)rate>>float
            let strokeThickness=scale 1
            let drawStation(x,y)=
                let size=scale 5
                let a=Ellipse()in a.Width<-size;a.Height<-size;a.StrokeThickness<-strokeThickness;a.Stroke<-Brushes.Black;a.Fill<-Brushes.White
                a.SetValue(Canvas.LeftProperty,scale x-size/2.0);a.SetValue(Canvas.TopProperty,scale y-size/2.0)
                a:>UIElement
            let drawLine i a=
                let colors=[Brushes.Gold;Brushes.Red;Brushes.DarkBlue]
                let polyline points=let a=Polyline()in a.Points<-points;a.StrokeThickness<-strokeThickness;a.Stroke<-colors.[i];a
                List.map(fun(a,b)->Point(scale a,scale b))a|>PointCollection|>polyline:>UIElement
            let drawMetro(stations,lines)a=
                let addAll(a:Canvas)b=b|>List.iter(fun b->a.Children.Add b|>ignore);a
                List.mapi drawLine lines@List.map drawStation stations|>addAll a
            let canvas(width,height)=let a=Canvas()in a.Width<-scale width;a.Height<-scale height;a
            let surface size=
                let a=canvas size
                a.Background<-Brushes.WhiteSmoke
                a
            surface size|>drawMetro metro
        let load(w:Window)=
            let game=Presentation.sample
            w.Content<-draw game
    open Draw
    let buildMainWindow _=
        let window title=Window(Title=title,(*FontFamily=FontFamily"Microsoft YaHei UI Light",*)ResizeMode=ResizeMode.NoResize,SizeToContent=SizeToContent.Height)
        let w=window"Test"
        load w
        w