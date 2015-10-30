using System;
using System.Drawing;

namespace GKCommon
{
	/// <summary>
	/// Description of ColorUtils.
	/// </summary>
	public static class ColorUtils
	{
		public static Color darker(Color color, float fraction)
		{
			float factor = (1.0f - fraction);

			int rgb = color.ToArgb();
			int red = (rgb >> 16) & 0xFF;
			int green = (rgb >> 8) & 0xFF;
			int blue = (rgb >> 0) & 0xFF;
			//int alpha = (rgb >> 24) & 0xFF;

			red = (int) (red * factor);
			green = (int) (green * factor);
			blue = (int) (blue * factor);

			red = (red < 0) ? 0 : red;
			green = (green < 0) ? 0 : green;
			blue = (blue < 0) ? 0 : blue;

			return Color.FromArgb(red, green, blue);
		}

		public static Color lighter(Color color, float fraction)
		{
			float factor = (1.0f + fraction);
			
			int rgb = color.ToArgb();
			int red = (rgb >> 16) & 0xFF;
			int green = (rgb >> 8) & 0xFF;
			int blue = (rgb >> 0) & 0xFF;
			//int alpha = (rgb >> 24) & 0xFF;

			red = (int) (red * factor);
			green = (int) (green * factor);
			blue = (int) (blue * factor);

			if (red < 0) {
				red = 0;
			} else if (red > 255) {
				red = 255;
			}
			if (green < 0) {
				green = 0;
			} else if (green > 255) {
				green = 255;
			}
			if (blue < 0) {
				blue = 0;
			} else if (blue > 255) {
				blue = 255;
			}

			//int alpha = color.getAlpha();

			return Color.FromArgb(red, green, blue);
		}
	}
}
