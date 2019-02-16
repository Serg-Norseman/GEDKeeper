/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.IO;
using System.Reflection;
using GKCore;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class FileFormatTests
    {
        [Test]
        public void Test_GK_UTF8()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_gk_utf8.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    GEDCOMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Иван Иванович Иванов", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_Ahnenblatt_ANSI_Win1250()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_ahn_ansi(win1250).ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    GEDCOMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Stanisław Nowak", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_Agelong_PseudoAnsel_Win1251()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_agelong_ansel(win1251).ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    GEDCOMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Иван ОВЕЧКИН", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_FTB6_ANSI_Win1251()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_ftb6_ansi(win1251).ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    GEDCOMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Иван Васильевич Петров", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_StdGEDCOM_Notes()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_stdgedcom_notes.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    GEDCOMNoteRecord noteRec1 = ctx.Tree.XRefIndex_Find("N1") as GEDCOMNoteRecord;
                    Assert.IsNotNull(noteRec1);
                    Assert.AreEqual("Test1\r\ntest2\r\ntest3", noteRec1.Note.Text);

                    GEDCOMNoteRecord noteRec2 = ctx.Tree.XRefIndex_Find("N2") as GEDCOMNoteRecord;
                    Assert.IsNotNull(noteRec2);
                    Assert.AreEqual("Test\r\ntest2\r\ntest3", noteRec2.Note.Text);
                }
            }
        }

        public void Test_TrueAnsel()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_ansel.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);
                }
            }
        }

        [Test]
        public void Test_FTB_BadLine()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_ftb_badline.ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    GEDCOMNoteRecord noteRec1 = ctx.Tree.XRefIndex_Find("N1") as GEDCOMNoteRecord;
                    Assert.IsNotNull(noteRec1);
                    Assert.AreEqual("Test1\r\ntest2\r\ntest3 badline badline badline badline", noteRec1.Note.Text);
                }
            }
        }
    }
}
