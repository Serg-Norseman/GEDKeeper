/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
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
using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using SkiaSharp;
using Xamarin.Essentials;
using Xamarin.Forms;

namespace GKUI.Components
{
    public static class UIHelper
    {
        public static Rectangle Rt2Rt(ExtRect ert)
        {
            return new Rectangle(ert.Left, ert.Top, ert.GetWidth(), ert.GetHeight());
        }

        public static ExtRect Rt2Rt(Rectangle ert)
        {
            return ExtRect.CreateBounds((int)ert.Left, (int)ert.Top, (int)ert.Width, (int)ert.Height);
        }

        public static SKRect Rt2SkRt(ExtRect ert)
        {
            return new SKRect(ert.Left, ert.Top, ert.GetWidth(), ert.GetHeight());
        }

        public static ExtRect SkRt2Rt(SKRect ert)
        {
            return ExtRect.CreateBounds((int)ert.Left, (int)ert.Top, (int)ert.Width, (int)ert.Height);
        }

        public static IColor ConvertColor(Color color)
        {
            return new ColorHandler(color);
        }

        public static Color ConvertColor(IColor color)
        {
            return ((ColorHandler)color).Handle;
        }

        public static Color Darker(Color color, float fraction)
        {
            int rgb = (int)(ColorExtensions.ToUInt(color) & 0xffffff);
            int newColor = GfxHelper.Darker(rgb, fraction);
            return Color.FromUint((uint)newColor);
        }

        public static Color Lighter(Color color, float fraction)
        {
            int rgb = (int)(ColorExtensions.ToUInt(color) & 0xffffff);
            int newColor = GfxHelper.Lighter(rgb, fraction);
            return Color.FromUint((uint)newColor);
        }

        public static ImageSource LoadResourceImage(string resName)
        {
            return ImageSource.FromResource(resName, typeof(GKUtils).Assembly);
        }

        public static ImageSource LoadResourceImage(Type baseType, string resName)
        {
            return ImageSource.FromResource(resName, baseType.Assembly);
        }
    }
}
