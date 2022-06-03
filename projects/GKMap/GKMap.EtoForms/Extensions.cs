using Eto.Drawing;

namespace GKMap.EtoForms
{
    public static class Extensions
    {
        public static Color FromArgb(int alpha, Color color)
        {
            return Color.FromArgb(color.Rb, color.Gb, color.Bb, alpha);
        }

        public static Point ToPoint(this PointF ptf)
        {
            return new Point((int)ptf.X, (int)ptf.Y);
        }
    }
}
