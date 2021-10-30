namespace GKMap
{
    public interface IMapMarker : IMapObject
    {
        GPoint LocalPosition { get; set; }
        GPoint Offset { get; set; }
        PointLatLng Position { get; set; }
        string ToolTipText { get; set; }
    }
}
