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

#if !__MonoCS__

using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Forms;
using GKUI.Providers;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class NoteEditDlgExTests : CustomWindowTest
    {
        private GEDCOMNoteRecord fNoteRecord;
        private IBaseWindow fBase;
        private NoteEditDlgEx fDialog;

        public override void Setup()
        {
            base.Setup();

            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fNoteRecord = new GEDCOMNoteRecord(fBase.Context.Tree, fBase.Context.Tree);

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

            var txtNote = new TextBoxTester("txtNote");
            txtNote.Enter("sample text");
            Assert.AreEqual("sample text", txtNote.Text);

            //ClickToolStripMenuItem("miExport", fDialog);
            //ClickToolStripMenuItem("miImport", fDialog);

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fNoteRecord.Note.Text);
        }
    }
}

#endif
