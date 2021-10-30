using System;

namespace GKMap
{
    public interface IMapObject : IDisposable
    {
        bool IsHitTestVisible { get; set; }

        bool IsMouseOver { get; set; }

        bool IsVisible { get; set; }

        bool IsInside(int x, int y);
    }
}
