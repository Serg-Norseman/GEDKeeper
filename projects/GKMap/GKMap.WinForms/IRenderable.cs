using System.Drawing;

namespace GKMap.WinForms
{
    public interface IRenderable
    {
        void OnRender(Graphics g);
    }
}
