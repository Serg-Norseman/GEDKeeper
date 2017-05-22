/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using GKCommon;
using GKCore.Interfaces;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class TypeHandlerTests
    {
        [Test]
        public void Test_General()
        {
            var handler = new ColorHandler(Color.PaleVioletRed);
            Assert.IsNotNull(handler);
            Assert.AreEqual(Color.PaleVioletRed, handler.Handle);
        }

        private sealed class ColorHandler: TypeHandler<Color>, IColor
        {
            public ColorHandler(Color handle) : base(handle)
            {
            }

            public IColor Darker(float fraction)
            {
                return new ColorHandler(Color.FromArgb(0));
            }

            public IColor Lighter(float fraction)
            {
                return new ColorHandler(Color.FromArgb(0));
            }

            public string GetName()
            {
                return string.Empty;
            }

            public int ToArgb()
            {
                return this.Handle.ToArgb();
            }

            public string GetCode()
            {
                string result = ToArgb().ToString("X6");
                return result;
            }

            public byte GetR()
            {
                return Handle.R;
            }

            public byte GetG()
            {
                return Handle.G;
            }

            public byte GetB()
            {
                return Handle.B;
            }

            public bool IsTransparent()
            {
                return (Handle == Color.Transparent);
            }
        }
    }
}
