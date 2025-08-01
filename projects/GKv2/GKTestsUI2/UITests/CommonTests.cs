﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Graphics;
using GKTests;
using GKUI.Platform.Handlers;
using NUnit.Framework;

namespace GKUI.Components
{
    [TestFixture]
    public class CommonTests
    {
        public CommonTests()
        {
            TestUtilsUI.InitUITest();
        }

        [Test]
        public void Test_UIHelper()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { UIHelper.CreateListView(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { UIHelper.CreateRecordsView(null, null, GDMRecordType.rtIndividual, false); });
        }

        [Test]
        public void Test_Other()
        {
            Rectangle rect1 = UIHelper.Rt2Rt(ExtRect.Empty);
            Assert.AreEqual(0, rect1.Left);
            Assert.AreEqual(0, rect1.Top);
            Assert.AreEqual(0, rect1.Right);
            Assert.AreEqual(0, rect1.Bottom);

            RectangleF rect2 = UIHelper.Rt2Rt(ExtRectF.Empty);
            Assert.AreEqual(0, rect2.Left);
            Assert.AreEqual(0, rect2.Top);
            Assert.AreEqual(0, rect2.Right);
            Assert.AreEqual(0, rect2.Bottom);
        }

        [Test]
        public void Test_ColorLD()
        {
            var color = AppHost.GfxProvider.CreateColor(0x646464);
            var chk_res = AppHost.GfxProvider.CreateColor(0x323232);
            Assert.AreEqual(((ColorHandler)chk_res).Handle, ((ColorHandler)color.Darker(0.5f)).Handle);

            color = AppHost.GfxProvider.CreateColor(GKColors.Silver);
            Assert.AreEqual("ffc0c0c0", color.GetName());

            color = AppHost.GfxProvider.CreateColor(0x323232);
            chk_res = AppHost.GfxProvider.CreateColor(0x4B4B4B);
            Assert.AreEqual(((ColorHandler)chk_res).Handle, ((ColorHandler)color.Lighter(0.5f)).Handle);
            Assert.AreEqual(75, chk_res.GetR());
            Assert.AreEqual(75, chk_res.GetG());
            Assert.AreEqual(75, chk_res.GetB());
            Assert.AreEqual(255, chk_res.GetA());
            Assert.IsFalse(chk_res.IsTransparent());
            Assert.AreEqual("ff4b4b4b", chk_res.GetName());
        }

        [Test]
        public void Test_Brush()
        {
            // FIXME
            /*var color = AppHost.GfxProvider.CreateColor(0x323232);
            var brush = AppHost.GfxProvider.CreateBrush(color);
            Assert.AreEqual(((ColorHandler)color).Handle, ((ColorHandler)brush.Color).Handle);*/
        }

        [Test]
        public void Test_Pen()
        {
            // FIXME
            /*var color = AppHost.GfxProvider.CreateColor(0x323232);
            var pen = AppHost.GfxProvider.CreatePen(color, 1.0f);
            Assert.AreEqual(((ColorHandler)color).Handle, ((ColorHandler)pen.Color).Handle);
            Assert.AreEqual(1.0f, pen.Width);*/
        }

        [Test]
        public void Test_Font()
        {
            string fontName;
            fontName = "Verdana";
            var fnt = AppHost.GfxProvider.CreateFont(fontName, 10, true);
            Assert.AreEqual(fontName, fnt.FontFamilyName);
            Assert.AreEqual(fontName, fnt.Name);
            Assert.AreEqual(10.0f, fnt.Size);
        }
    }
}
