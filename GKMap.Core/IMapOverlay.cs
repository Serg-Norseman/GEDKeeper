using System;

namespace GKMap
{
    public interface IMapOverlay : IDisposable
    {
        string Id { get; set; }

        bool IsHitTestVisible { get; set; }

        bool IsZoomSignificant { get; set; }

        bool IsVisible { get; set; }

        ObservableCollection<IMapMarker> Markers { get; }

        ObservableCollection<IMapRoute> Routes { get; }

        ObservableCollection<IMapPolygon> Polygons { get; }
    }
}
