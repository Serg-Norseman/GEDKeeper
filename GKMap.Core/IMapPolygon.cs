namespace GKMap
{
    public interface IMapPolygon : IMapFigure
    {
        bool IsInsideLatLng(PointLatLng p);
    }
}
