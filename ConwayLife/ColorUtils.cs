/*
 *  Conway's Game of Life.
 *  Copyright (C) 2009-2015 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "ConwayLifeGame".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;

namespace ConwayLife
{
	/// <summary>
	/// 
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
