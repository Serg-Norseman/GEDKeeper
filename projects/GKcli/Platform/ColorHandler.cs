/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using BSLib;
using GKCore.Design.Graphics;

namespace GKUI.Platform
{
    public sealed class ColorHandler : TypeHandler<ConsoleColor>, IColor
    {
        public ColorHandler(ConsoleColor handle) : base(handle)
        {
        }

        public IColor Darker(float fraction)
        {
            return this;
        }

        public IColor Lighter(float fraction)
        {
            return this;
        }

        public string GetName()
        {
            ConsoleColor color = this.Handle;
            return color.ToString(); // Name;
        }

        public int ToArgb()
        {
            ConsoleColor color = this.Handle;
            var trueColor = TrueColorMap.Where(c => c.Value == color).Select(c => c.Key).FirstOrDefault();
            int result = GfxHelper.MakeArgb(255, trueColor.Red, trueColor.Green, trueColor.Blue);
            return result;
        }

        public string GetCode()
        {
            int argb = ToArgb() & 0xFFFFFF;
            string result = argb.ToString("X6");
            return result;
        }

        public byte GetR()
        {
            return 0;
        }

        public byte GetG()
        {
            return 0;
        }

        public byte GetB()
        {
            return 0;
        }

        public byte GetA()
        {
            return 0;
        }

        public bool IsTransparent()
        {
            return false;
        }

        public struct TrueColor
        {
            public byte Red;
            public byte Green;
            public byte Blue;

            public TrueColor(byte red, byte green, byte blue)
            {
                Red = red; Green = green; Blue = blue;
            }
        }

        public static ConsoleColor ToConsoleColor(byte r, byte g, byte b)
        {
            var newColor = new TrueColor(r, g, b);
            var distances = TrueColorMap.Select(k => Tuple.Create(k.Key, CalculateDistance(k.Key, newColor)));
            var match = distances.OrderBy(t => t.Item2).First();
            return TrueColorMap[match.Item1];
        }

        private static double CalculateDistance(TrueColor color1, TrueColor color2)
        {
            // use RGB distance
            var rmean = ((float)color1.Red + color2.Red) / 2;
            var r = color1.Red - color2.Red;
            var g = color1.Green - color2.Green;
            var b = color1.Blue - color2.Blue;
            return Math.Sqrt(
                ((int)((512 + rmean) * r * r) >> 8)
                + (4 * g * g)
                + ((int)((767 - rmean) * b * b) >> 8));
        }


        private static readonly Dictionary<TrueColor, ConsoleColor> TrueColorMap = new Dictionary<TrueColor, ConsoleColor>() {
            { new TrueColor (0,0,0), ConsoleColor.Black},
            { new TrueColor (0, 0, 0x80), ConsoleColor.DarkBlue},
            { new TrueColor (0, 0x80, 0), ConsoleColor.DarkGreen},
            { new TrueColor (0, 0x80, 0x80), ConsoleColor.DarkCyan},
            { new TrueColor (0x80, 0, 0), ConsoleColor.DarkRed},
            { new TrueColor (0x80, 0, 0x80), ConsoleColor.DarkMagenta},
            { new TrueColor (0xC1, 0x9C, 0x00), ConsoleColor.DarkYellow},
            { new TrueColor (0xC0, 0xC0, 0xC0), ConsoleColor.Gray},
            { new TrueColor (0x80, 0x80, 0x80), ConsoleColor.DarkGray},
            { new TrueColor (0, 0, 0xFF), ConsoleColor.Blue},
            { new TrueColor (0, 0xFF, 0), ConsoleColor.Green},
            { new TrueColor (0, 0xFF, 0xFF), ConsoleColor.Cyan},
            { new TrueColor (0xFF, 0, 0), ConsoleColor.Red},
            { new TrueColor (0xFF, 0, 0xFF), ConsoleColor.Magenta },
            { new TrueColor (0xFF, 0xFF, 0), ConsoleColor.Yellow},
            { new TrueColor (0xFF, 0xFF, 0xFF), ConsoleColor.White},
        };
    }
}

