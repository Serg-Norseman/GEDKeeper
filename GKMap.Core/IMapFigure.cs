using System.Collections.Generic;

namespace GKMap
{
    public interface IMapFigure : IMapObject
    {
        bool HasLines { get; }

        List<GPoint> LocalPoints { get; }

        List<PointLatLng> Points { get; }

        void UpdateGraphicsPath();
    }
}
