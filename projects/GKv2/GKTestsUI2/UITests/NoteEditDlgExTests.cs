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

#if !DIS_NUF

using GDModel;
using GKCore.Design;
using GKTests;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class NoteEditDlgExTests : CustomWindowTest
    {
        private GDMNoteRecord fNoteRecord;
        private IBaseWindow fBase;
        private NoteEditDlgEx fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fNoteRecord = new GDMNoteRecord(fBase.Context.Tree);

            fDialog = new NoteEditDlgEx(fBase);
            fDialog.NoteRecord = fNoteRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fNoteRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fNoteRecord, fDialog.NoteRecord);

            ClickToolStripButton("btnBold", fDialog);
            ClickToolStripButton("btnItalic", fDialog);
            ClickToolStripButton("btnUnderline", fDialog);
            ClickToolStripButton("btnURL", fDialog);

            ClickToolStripMenuItem("miSelectAndCopy", fDialog);
            ClickToolStripMenuItem("miClear", fDialog);

            EnterRichText("txtNote", fDialog, "sample text");

            //ClickToolStripMenuItem("miExport", fDialog);
            //ClickToolStripMenuItem("miImport", fDialog);

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fNoteRecord.Lines.Text);
        }
    }
}

#endif
