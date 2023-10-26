
using SkiaSharp;

namespace GKMap.Xamarin
{
    public static class Extensions
    {
        public static SKColor FromArgb(int alpha, SKColor color)
        {
            return new SKColor(color.Red, color.Green, color.Blue, (byte)alpha);
        }
    }
}
